# aks-node-termination-handler (aksnth)

Gracefully handle Azure Virtual Machine (Spot instance) shutdown within Kubernetes

_Inspired by [aws/aws-node-termination-handler](https://github.com/aws/aws-node-termination-handler)
and [maksim-paskal/aks-node-termination-handler](https://github.com/maksim-paskal/aks-node-termination-handler) - kudos
to you guys!_

## Installation

`aksnth` can be installed using helm

```
$ helm repo add aksnth https://fridayy.github.io/aks-node-termination-handler/
$ helm repo update
$ helm install aksnth aksnth/aks-node-termination-handler -n aksnth
```

## Features

* Runs on Azure Spot Instance Kubernetes nodes
* Polls the Azure Metadata Service
  for [Scheduled Terminal Events](https://learn.microsoft.com/en-us/azure/virtual-machines/linux/scheduled-events)
* Creates a corresponding `v1beta.Event` [Kubernetes event](#event)
* [Safely drains the node](#draining-the-node)

### Event

If a scheduled event is received `aksnth` emits a kubernetes event in the following format:

```
LAST SEEN   TYPE      REASON               OBJECT                                          MESSAGE
9m59s       Warning   Unhealthy            node/aios                                       Node 'aios' will change into status 'Preempt' [Not before: 'Mon, 19 Sep 2016 18:29:47 GMT']
```

### Draining the node

`aksnth` [safely drains](https://kubernetes.io/docs/tasks/administer-cluster/safely-drain-node/) the affected node by

* Adding a `NoSchedule` taint on the node
* [Cordoning](https://kubernetes.io/docs/concepts/architecture/nodes/#manual-node-administration) the node
* Creating [PodEviction](https://kubernetes.io/docs/concepts/scheduling-eviction/api-eviction/) objects

## Metrics

`aksnth` exposes the following metrics on the `/metrics` endpoint:

```
# TYPE aksnth_spot_node_available gauge
aksnth_spot_node_available{
   container="aks-node-termination-handler", 
   endpoint="http", 
   instance="10.244.0.2:8080", 
   job="aksnth/aksnth-aks-node-termination-handler", 
   namespace="aksnth", 
   node="aks-somespotpool-38258973-vmss000001", 
   pod="aksnth-aks-node-termination-handler-9jztg"
} 1
```

`aksnth_spot_node_available` is `1` until the node receives an eviction event. After that the value will be `0`.

```
# TYPE aksnth_spot_node_eviction_imminent gauge
aksnth_spot_node_eviction_imminent{
  container="aks-node-termination-handler", 
  endpoint="http", 
  instance="10.244.0.2:8080", 
  job="aksnth/aksnth-aks-node-termination-handler", 
  namespace="aksnth", 
  node="aks-dgsc4m16z1-38258973-vmss000001", 
  pod="aksnth-aks-node-termination-handler-9jztg"
} 0
```

`aksnth_spot_node_eviction_imminent` is `0` until the node receives an eviction event. After that the value will be `1`.



## Configuration

| Name                     | Description                                                                                       | Value           |
|--------------------------|---------------------------------------------------------------------------------------------------|-----------------|
| `mock.enabled`           | In mock mode scheduled preempt events can be simulated by using the `/simulate-eviction` endpoint | `false`         |
| `poll.interval`          | The interval (in milliseconds) used for polling the scheduled events API                          | `1000`          |
| `podMonitor.enabled`     | Specfies if a promtheus pod monitor should be created                                             | `true`          |
| `podMonitor.interval`    | Configures the pod monitor scrape interval to be used                                             | `15s`           |
| `podMonitor.labels`      | Additional pod monitor labels                                                                     | `{}`            |


