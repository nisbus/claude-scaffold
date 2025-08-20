#!/usr/bin/env node

/**
 * Update MCP Configuration
 * Adds missing MCP servers to the Claude Code configuration
 */

const fs = require('fs');
const path = require('path');
const os = require('os');

const MCP_CONFIG_PATH = path.join(os.homedir(), '.claude', 'mcp_settings.json');
const TEMPLATE_CONFIG_PATH = path.join(__dirname, '..', 'config', 'mcp_settings.json');

function loadConfig(filePath) {
  try {
    return JSON.parse(fs.readFileSync(filePath, 'utf8'));
  } catch (error) {
    return { mcpServers: {} };
  }
}

function saveConfig(filePath, config) {
  const dir = path.dirname(filePath);
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
  fs.writeFileSync(filePath, JSON.stringify(config, null, 2));
}

function updateMcpConfig(serversToAdd) {
  console.log('📝 Updating MCP configuration...');
  
  // Load current config
  const currentConfig = loadConfig(MCP_CONFIG_PATH);
  const templateConfig = loadConfig(TEMPLATE_CONFIG_PATH);
  
  // Add missing servers
  let updated = false;
  for (const server of serversToAdd) {
    if (!currentConfig.mcpServers[server] && templateConfig.mcpServers[server]) {
      console.log(`  Adding ${server} MCP server...`);
      currentConfig.mcpServers[server] = templateConfig.mcpServers[server];
      updated = true;
    }
  }
  
  if (updated) {
    // Save updated config
    saveConfig(MCP_CONFIG_PATH, currentConfig);
    console.log('✅ MCP configuration updated');
    console.log('⚠️  Please restart Claude Code to load new MCP servers');
    return true;
  } else {
    console.log('ℹ️  All required MCP servers already configured');
    return false;
  }
}

// Get servers to add from command line arguments
const serversToAdd = process.argv.slice(2);

if (serversToAdd.length === 0) {
  // Default to adding all required servers
  const requiredServers = ['render', 'github', 'stripe', 'playwright'];
  updateMcpConfig(requiredServers);
} else {
  updateMcpConfig(serversToAdd);
}

module.exports = { updateMcpConfig };