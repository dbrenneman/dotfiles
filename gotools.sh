#!/bin/sh

echo "Installing gopls..."
GO111MODULE=on go get golang.org/x/tools/gopls@latest

echo "Installing goconvey..."
go get -u github.com/smartystreets/goconvey

echo "Installing goimports..."
go get -u golang.org/x/tools/cmd/goimports

echo "Installing gocode..."
go get -u github.com/stamblerre/gocode

echo "Installing guru..."
go get -u golang.org/x/tools/cmd/guru
go build golang.org/x/tools/cmd/guru
mv guru $(go env GOPATH)/bin
