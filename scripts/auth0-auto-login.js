#!/usr/bin/env node

/**
 * Auth0 CLI Automated Login with Playwright
 * 
 * This script automates the Auth0 CLI login process by:
 * 1. Starting the auth0 login command
 * 2. Detecting when the browser opens
 * 3. Automatically clicking the "Authorize" button
 */

const { chromium } = require('playwright');
const { spawn } = require('child_process');
const path = require('path');

// Configuration
const AUTH0_LOGIN_TIMEOUT = 30000; // 30 seconds
const BROWSER_WAIT_TIME = 3000; // 3 seconds to wait for browser to open

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function findAuth0Window() {
  try {
    // Try to connect to existing browser instances
    const browser = await chromium.connectOverCDP('http://localhost:9222');
    const pages = await browser.pages();
    
    for (const page of pages) {
      const url = page.url();
      if (url.includes('auth0.com') || url.includes('authorize')) {
        return { browser, page };
      }
    }
    
    return null;
  } catch (error) {
    return null;
  }
}

async function automateAuth0Login() {
  log('🔐 Starting Auth0 CLI automated login...', 'blue');
  
  // Start the auth0 login process
  log('  Starting auth0 login command...', 'yellow');
  const auth0Process = spawn('auth0', ['login'], {
    stdio: 'inherit',
    shell: true
  });
  
  // Wait for browser to open
  log('  Waiting for browser to open...', 'yellow');
  await sleep(BROWSER_WAIT_TIME);
  
  let browser = null;
  let page = null;
  let loginSuccessful = false;
  
  try {
    // Launch Playwright browser to handle the Auth0 login
    browser = await chromium.launch({
      headless: false, // We need to see what's happening
      args: ['--disable-blink-features=AutomationControlled']
    });
    
    // Monitor for new pages/popups
    browser.on('targetcreated', async (target) => {
      const targetPage = await target.page();
      if (targetPage) {
        const url = targetPage.url();
        if (url.includes('auth0.com')) {
          page = targetPage;
        }
      }
    });
    
    // Check for existing Auth0 pages
    const pages = await browser.pages();
    for (const p of pages) {
      const url = p.url();
      if (url.includes('auth0.com')) {
        page = p;
        break;
      }
    }
    
    // If no Auth0 page found, wait and check periodically
    let attempts = 0;
    while (!page && attempts < 10) {
      log('  Looking for Auth0 authorization page...', 'yellow');
      await sleep(2000);
      
      // Check all browser contexts
      const contexts = browser.contexts();
      for (const context of contexts) {
        const contextPages = await context.pages();
        for (const p of contextPages) {
          const url = p.url();
          if (url.includes('auth0.com') || url.includes('authorize')) {
            page = p;
            break;
          }
        }
        if (page) break;
      }
      
      attempts++;
    }
    
    if (!page) {
      // Try opening the URL directly if we can find it from the CLI output
      log('  Auth0 page not detected, checking for manual URL...', 'yellow');
      
      // Alternative: Look for the URL in clipboard or terminal output
      const clipboardText = await getClipboardContent();
      if (clipboardText && clipboardText.includes('auth0.com')) {
        page = await browser.newPage();
        await page.goto(clipboardText);
      }
    }
    
    if (page) {
      log('  Auth0 page found! Automating authorization...', 'green');
      
      // Wait for the page to load
      await page.waitForLoadState('networkidle', { timeout: 10000 }).catch(() => {});
      
      // Try multiple selectors for the authorize button
      const authorizeSelectors = [
        'button:has-text("Authorize")',
        'button:has-text("Allow")',
        'button:has-text("Accept")',
        'button[type="submit"]',
        'input[type="submit"]',
        '#allow',
        '.authorize-button',
        'button.btn-success',
        'button.btn-primary'
      ];
      
      let buttonClicked = false;
      for (const selector of authorizeSelectors) {
        try {
          const button = await page.$(selector);
          if (button) {
            const isVisible = await button.isVisible();
            if (isVisible) {
              log(`  Found authorization button with selector: ${selector}`, 'green');
              await button.click();
              buttonClicked = true;
              break;
            }
          }
        } catch (error) {
          // Continue to next selector
        }
      }
      
      if (!buttonClicked) {
        log('  Authorization button not found automatically', 'yellow');
        log('  Please click the "Authorize" button manually in the browser', 'yellow');
        
        // Wait for navigation after manual click
        await page.waitForNavigation({ timeout: AUTH0_LOGIN_TIMEOUT }).catch(() => {});
      } else {
        log('  Authorization button clicked!', 'green');
        
        // Wait for the authorization to complete
        await page.waitForNavigation({ timeout: 10000 }).catch(() => {});
      }
      
      // Check if login was successful by looking for success indicators
      const currentUrl = page.url();
      if (currentUrl.includes('success') || currentUrl.includes('callback') || currentUrl.includes('localhost')) {
        loginSuccessful = true;
        log('  ✅ Auth0 authorization successful!', 'green');
      }
      
      // Wait a bit for the CLI to process the callback
      await sleep(2000);
      
    } else {
      log('  ⚠️  Could not find Auth0 authorization page', 'red');
      log('  Please complete the authorization manually in your browser', 'yellow');
    }
    
  } catch (error) {
    log(`  Error during automation: ${error.message}`, 'red');
    log('  Please complete the authorization manually', 'yellow');
  } finally {
    // Close the browser
    if (browser) {
      await browser.close();
    }
  }
  
  // Wait for the auth0 process to complete
  return new Promise((resolve, reject) => {
    auth0Process.on('exit', (code) => {
      if (code === 0 || loginSuccessful) {
        log('✅ Auth0 CLI login completed successfully!', 'green');
        resolve();
      } else {
        log(`❌ Auth0 CLI login failed with code ${code}`, 'red');
        reject(new Error(`Auth0 login failed with code ${code}`));
      }
    });
    
    // Timeout fallback
    setTimeout(() => {
      if (loginSuccessful) {
        log('✅ Auth0 CLI login completed (timeout)', 'green');
        resolve();
      } else {
        log('⏱️  Auth0 login timed out - please check auth0 whoami', 'yellow');
        resolve(); // Resolve anyway to continue
      }
    }, AUTH0_LOGIN_TIMEOUT);
  });
}

async function getClipboardContent() {
  try {
    const { exec } = require('child_process');
    return new Promise((resolve) => {
      if (process.platform === 'darwin') {
        exec('pbpaste', (err, stdout) => {
          resolve(err ? '' : stdout);
        });
      } else if (process.platform === 'linux') {
        exec('xclip -selection clipboard -o', (err, stdout) => {
          resolve(err ? '' : stdout);
        });
      } else {
        resolve('');
      }
    });
  } catch {
    return '';
  }
}

// Alternative approach using system automation
async function alternativeAuth0Login() {
  log('🔐 Starting alternative Auth0 login approach...', 'blue');
  
  try {
    // First, check if already logged in
    const { execSync } = require('child_process');
    try {
      execSync('auth0 whoami', { stdio: 'pipe' });
      log('✅ Already logged in to Auth0!', 'green');
      return;
    } catch {
      // Not logged in, continue
    }
    
    // Start auth0 login in background
    const auth0Process = spawn('auth0', ['login'], {
      stdio: 'pipe',
      shell: true
    });
    
    let loginUrl = '';
    
    // Capture the URL from output
    auth0Process.stdout.on('data', (data) => {
      const output = data.toString();
      console.log(output);
      
      // Look for the URL in the output
      const urlMatch = output.match(/https:\/\/[^\s]+/);
      if (urlMatch) {
        loginUrl = urlMatch[0];
      }
    });
    
    auth0Process.stderr.on('data', (data) => {
      console.error(data.toString());
    });
    
    // Wait a bit for URL to appear
    await sleep(3000);
    
    if (loginUrl) {
      log(`  Opening Auth0 login URL: ${loginUrl}`, 'blue');
      
      // Open browser with Playwright
      const browser = await chromium.launch({ headless: false });
      const page = await browser.newPage();
      await page.goto(loginUrl);
      
      // Wait for user to complete login
      log('  Please complete the login in the browser window...', 'yellow');
      
      // Monitor for successful callback
      page.on('framenavigated', async (frame) => {
        const url = frame.url();
        if (url.includes('localhost') && url.includes('callback')) {
          log('  ✅ Login callback detected!', 'green');
          setTimeout(() => browser.close(), 2000);
        }
      });
      
      // Wait for process to complete
      await new Promise((resolve) => {
        auth0Process.on('exit', resolve);
        setTimeout(resolve, AUTH0_LOGIN_TIMEOUT); // Timeout fallback
      });
      
    } else {
      log('  Could not capture login URL, please complete manually', 'yellow');
    }
    
  } catch (error) {
    log(`Error: ${error.message}`, 'red');
    throw error;
  }
}

// Main execution
async function main() {
  try {
    // Try the main approach
    await automateAuth0Login();
  } catch (error) {
    log('First approach failed, trying alternative...', 'yellow');
    try {
      await alternativeAuth0Login();
    } catch (altError) {
      log('❌ Automated login failed. Please run: auth0 login', 'red');
      process.exit(1);
    }
  }
  
  // Verify login was successful
  try {
    const { execSync } = require('child_process');
    const result = execSync('auth0 whoami', { stdio: 'pipe' });
    log('✅ Auth0 login verified successfully!', 'green');
    console.log(result.toString());
  } catch (error) {
    log('⚠️  Could not verify Auth0 login status', 'yellow');
  }
}

// Check if Playwright is installed
try {
  require('playwright');
} catch (error) {
  log('📦 Installing Playwright...', 'yellow');
  const { execSync } = require('child_process');
  execSync('npm install playwright', { stdio: 'inherit' });
  log('✅ Playwright installed', 'green');
}

// Run the script
if (require.main === module) {
  main().catch(error => {
    log(`Fatal error: ${error.message}`, 'red');
    process.exit(1);
  });
}

module.exports = { automateAuth0Login, alternativeAuth0Login };