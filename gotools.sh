#!/bin/sh

echo "Installing gopls..."
GO111MODULE=on go get golang.org/x/tools/gopls

echo "Installing golangci-lint..."
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.27.0

echo "Installing goconvey..."
go get github.com/smartystreets/goconvey

echo "Installing cover..."
go get golang.org/x/tools/cmd/cover

echo "Installing staticcheck..."
GO111MODULE=on go get honnef.co/go/tools

echo "Installing protoc-gen-go..."
go get google.golang.org/protobuf/cmd/protoc-gen-go
go install google.golang.org/protobuf/cmd/protoc-gen-go
