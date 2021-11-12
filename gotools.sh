#!/bin/sh

echo "Installing gopls..."
go install golang.org/x/tools/gopls@latest

echo "Installing goconvey..."
go install github.com/smartystreets/goconvey@latest
