
----------

> **关于 Janus 的历史**

> 在罗马神话中 Janus 是天门神，早晨打开天门，让阳光普照人间，晚上又把天门关上，使黑暗降临大地。他的头部前后各有一副面孔，同时看着两个不同方向，一副看着过去，一副看着未来，因此也称两面神，或被尊称为时间之神。罗马有好几座 Janus 神庙。Janus 掌管所有的出入门户，因此罗马人在战时永远将 Janus 神殿的门敞开着，以便军人在败阵时躲入殿内以求庇护，或是在战胜时凯旋入殿。早期的 Janus 神像的两副面孔都有胡子，后来没有胡子，但是一副面孔年轻，另一副面孔年老。Janus 的右手指上刻有数字 CCC（300），左手指上刻着数字 LXV（65），合在一起恰是一年的天数。从纪元前 1 世纪起，罗马人把祭祀 Janus 的节日和新年结合在一起。罗马的执政官也在元旦这一天就职，并向 Janus 献祭，祈求国家的安宁。为了纪念 Janus，罗马人把正月称为 Januarius（mensis），意含“Janus 之月”，英文借用了该词，先作 Januarie，后作 January 。而在英文吸收 January 一词之前，撒克逊人把正月叫作 Wulf-Monath（wolf-month），意为“狼月”，因为此时正值严冬，是狼群出没村子寻觅食物的时节。

----------

## 监督树总体结构



```
                                                      |
                                                   janus_app
                                                      |
                                                      | (one_for_one)
               +-------------------+------------------+--------------------+
               |                   |                  |                    |
               |                   |                  |                    |
             topman          janus_acceptor      (supervisor)            mapper
               |                                      |
               |                                      | (simple_one_for_one)
      +--------+-------|                     +--------+-------+
      |        |       |                     |        |       |
      |        |       |                     |        |       |
   pubsub     ...   pubsub                  ...   transport  ...
(topic:<<"T1">>)  (topic:<<"T2">>)                    |
                                                      | (janus_flash 封装层)
                                                      |
                                                 client_proxy

```

## subscriber 行为

```sequence
title: 订阅者行为
subscriber(flashbot)-->janus: ...TCP setup...
Note right of janus: a. janus_acceptor 创建 transport 进程
Note right of janus: b. 进而创建 client_proxy 进程
Note right of janus: c. 发送成功创建 client_proxy 的时间戳和 token 标识
janus->subscriber(flashbot): {timestamp:xxx, token:yyy},1
subscriber(flashbot)->janus: <regular-socket/>,0,PING,0
subscriber(flashbot)->janus: <regular-socket/>,0,{action:subscribe, data:events},0
Note right of janus: 内部经过 
Note right of janus: transport ->client_proxy -> topman -> pubsub
Note right of janus: 的处理，最终回复 subscribe 成功的 ack
janus->subscriber(flashbot): ACK,1
```


### janus 针对 subscribe 的内部处理

```sequence
title: xxx
subscriber-->transport: ...TCP setup...
subscriber->transport: ...{action:subscribe, data:Topic}...
transport->client_proxy: cast {Action, Topic}
client_proxy->topman: topman:subscribe(CProxyPid,Topic)
topman->topman: cast {subscribe, CProxyPid, Topic}
Note right of topman: [...
Note right of topman: 在没有与 Topic 关联的 pubsub 进程时，才有如下三步
topman-->pubsub: pubsub:start(Topic)
pubsub-->pubsub: 关联 Topic
topman-->pubsub: monitor
Note right of topman: ...] 
topman->pubsub: pubsub:subscribe(PSPid, CProxyPid)
pubsub->pubsub: cast {subscribe, CProxyPid}
pubsub->client_proxy: monitor
pubsub->client_proxy: ack
client_proxy->transport: ack
transport->subscriber: ACK,1
```




## publisher 行为

```sequence
title: 发布者行为
subscriber(flashbot)-->janus: ...TCP setup...
publisher(flashbot)-->janus: ...TCP setup...
publisher(flashbot)->janus: <regular-socket/>,0,PUBLISH,0,{topic:xxx,event:xxx,message_id:xxx,data:xxx}
Note right of janus: 内部经过
Note right of janus: transport -> topman -> pubsub -> client_proxy
Note right of janus: 的处理，最终将消息推送给 subscriber
janus->subscriber(flashbot): {timestamp:xxx, topic:Topic},1
Note right of janus: 在 publish 后，启动 30s 定时器
Note right of janus: 在 30s 内若没有新内容需要 publish，则触发 PING 发送
janus->subscriber(flashbot): PING,1
subscriber(flashbot)->janus: PONG,0
```


### janus 针对 publish 的内部处理

```sequence
title: xxx
publisher-->transport: ...TCP setup...
publisher->transport: ...PUBLISH,0,{topic:Topic,...}...
transport->topman: topman:publish(Msg, Topic)
topman->topman: abcast {publish, Msg, Topic}
Note right of topman: [...
Note right of topman: 在没有与 Topic 关联的 pubsub 进程时，才有如下三步
topman-->pubsub: pubsub:start(Topic)
pubsub-->pubsub: 关联 Topic
topman-->pubsub: monitor
Note right of topman: ...] 
topman->pubsub: pubsub:publish(PSPid, Msg)
pubsub->pubsub: cast {publish, Msg}

pubsub->client_proxy_n: {message,Msg_1}
client_proxy_n->subscriber_n: {timestamp:xx, topic:Topic,...},1


```