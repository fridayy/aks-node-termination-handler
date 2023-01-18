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
* Optionally pushes a metric via `HTTP POST` to a
  configured [promtheus push gateway](https://github.com/prometheus/pushgateway)

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

## prometheus pushgateway

If condfigured `aksnth` pushes a counter metric to a prometheus pushgateway in case of an eviction event:

```
# TYPE spot_instance_eviction_count counter
spot_instance_eviction_count{
 job = "aks-node-termination-handler", 
 node = "aks-somespot-29758349-vmss_9",
 instance = "aksnth-aks-node-termination-handler-dms9n"} 1
```

## Configuration

| Name                     | Description                                                                                       | Value           |
|--------------------------|---------------------------------------------------------------------------------------------------|-----------------|
| `mock.enabled`           | In mock mode scheduled preempt events can be simulated by using the `/simulate-eviction` endpoint | `false`         |
| `poll.interval`          | The interval (in milliseconds) used for polling the scheduled events API                          | `1000`          |
| `pushgateway.enabled`    | Enables pushing a corresponding eviction metric to a prometheus pushgateway                       | `false`            |
| `pushgateway.url`        | Metrics URL of the prometheus push gateway (e.g `pushgateway:9091.monitoring.svc.cluster.local/metrics`)                                                   | `""`            |

