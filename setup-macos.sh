#!/bin/zsh

# setup-macos.sh — Opinionated macOS defaults for terminal-focused development
# Run once on a fresh machine, then log out and back in for all changes to take effect.

echo "Configuring macOS defaults..."

# ============================================================================
# Keyboard
# ============================================================================

# Fast key repeat rate (lower = faster, 2 is the fastest via UI, 1 is faster)
defaults write NSGlobalDomain KeyRepeat -int 2

# Short delay before key repeat starts (lower = shorter, 15 is shortest via UI)
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable press-and-hold for accents in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Disable auto-capitalize
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

# Disable smart quotes (they break code)
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes (they break code)
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable automatic period substitution (double-space → period)
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

# Enable full keyboard access for all controls (Tab in dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# ============================================================================
# Dock
# ============================================================================

# Auto-hide the dock
defaults write com.apple.dock autohide -bool true

# Remove auto-hide delay
defaults write com.apple.dock autohide-delay -float 0

# Faster auto-hide animation
defaults write com.apple.dock autohide-time-modifier -float 0.3

# Smaller dock icons
defaults write com.apple.dock tilesize -int 36

# Don't show recent applications
defaults write com.apple.dock show-recents -bool false

# Minimize windows using scale effect (faster than genie)
defaults write com.apple.dock mineffect -string "scale"

# Don't minimize windows into application icon
defaults write com.apple.dock minimize-to-application -bool false

# ============================================================================
# Finder
# ============================================================================

# Show hidden files
defaults write com.apple.finder AppleShowAllFiles -bool true

# Show all file extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Default to list view
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Avoid creating .DS_Store files on USB volumes
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# ============================================================================
# Screenshots
# ============================================================================

# Save screenshots to ~/Screenshots
mkdir -p "${HOME}/Screenshots"
defaults write com.apple.screencapture location -string "${HOME}/Screenshots"

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

# Save screenshots as PNG
defaults write com.apple.screencapture type -string "png"

# ============================================================================
# Trackpad
# ============================================================================

# Enable tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Fast tracking speed (0.0 to 3.0)
defaults write NSGlobalDomain com.apple.trackpad.scaling -float 2.5

# Enable three-finger drag
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true

# ============================================================================
# Dialogs and Windows
# ============================================================================

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Disable the "Are you sure you want to open this application?" dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Reduce transparency for better performance
defaults write com.apple.universalaccess reduceTransparency -bool true

# ============================================================================
# Misc
# ============================================================================

# Disable Spotlight indexing for development directories
# (add paths to Privacy list in System Settings > Spotlight manually)

# Disable the crash reporter
defaults write com.apple.CrashReporter DialogType -string "none"

# Disable disk image verification
defaults write com.apple.frameworks.diskimages skip-verify -bool true
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

# ============================================================================
# Apply changes
# ============================================================================

killall Dock 2>/dev/null
killall Finder 2>/dev/null
killall SystemUIServer 2>/dev/null

echo "Done. Some changes require a logout/restart to take effect."
