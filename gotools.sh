#!/bin/sh

echo "Installing gopls..."
go install golang.org/x/tools/gopls@latest

echo "Installing goconvey..."
go install github.com/smartystreets/goconvey@latest

GO111MODULE=on CGO_ENABLED=0 go install -v -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install github.com/cweill/gotests/...@latest
go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install github.com/fatih/gomodifytags@latest
go install github.com/godoctor/godoctor@latest
go install github.com/haya14busa/gopkgs/cmd/gopkgs@latest
go install github.com/josharian/impl@latest
go install github.com/rogpeppe/godef@latest
