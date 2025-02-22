#!/usr/bin/env sh

go install github.com/x-motemen/gore/cmd/gore@latest &&
    go install golang.org/x/tools/cmd/godoc@latest && \
    go install github.com/fatih/gomodifytags@latest && \
    go install github.com/stamblerre/gocode@latest && \
    go install golang.org/x/tools/cmd/goimports@latest && \
    go install golang.org/x/tools/cmd/gorename@latest && \
    go install github.com/cweill/gotests/gotests@latest && \
    go install golang.org/x/tools/gopls@latest && \
    go install github.com/go-delve/delve/cmd/dlv@latest && \
    go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
