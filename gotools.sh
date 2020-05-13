#!/bin/sh

echo "Installing gopls..."
GO111MODULE=on go get golang.org/x/tools/gopls@v0.4.1-pre2

echo "Installing golangci-lint..."
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.26.0

echo "Installing goconvey..."
go get -u github.com/smartystreets/goconvey

echo "Installing cover..."
go get golang.org/x/tools/cmd/cover
