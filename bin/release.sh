#!/usr/bin/env bash
set -eou pipefail

update_charts() {
  RELEASE_VERSION=$(echo "${GITHUB_REF}" | sed -e "s/refs\/tags\/v//g")
  echo "Release: ${RELEASE_VERSION}"
  sed -e "s/9.9.9/${RELEASE_VERSION}/g" < charts/aks-node-termination-handler/Chart.yaml > charts/aks-node-termination-handler/Chart.new.yaml
  mv charts/aks-node-termination-handler/Chart.new.yaml charts/aks-node-termination-handler/Chart.yaml
  echo "Charts updated"
}

helm_prepare() {
  cd charts
  helm lint aks-node-termination-handler/
  helm package aks-node-termination-handler/
}

update_charts
helm_prepare