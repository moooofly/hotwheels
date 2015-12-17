
----------

## 一句话概述

- Janus 是一种消息服务器，专门被优化以便能够处理，以 TCP 单播方式同时发消息给数以千计客户端的场景。这些客户端可以订阅到各种 topic 上。 
- 终极目标是能够在 20k 客户端同时在线的情况下，进行消息推送时保证延时低于 2 秒。 

----------

## 历史

> **关于 Janus 的历史**

> 在罗马神话中 Janus 是天门神，早晨打开天门，让阳光普照人间，晚上又把天门关上，使黑暗降临大地。他的头部前后各有一副面孔，同时看着两个不同方向，一副看着过去，一副看着未来，因此也称两面神，或被尊称为时间之神。罗马有好几座 Janus 神庙。Janus 掌管所有的出入门户，因此罗马人在战时永远将 Janus 神殿的门敞开着，以便军人在败阵时躲入殿内以求庇护，或是在战胜时凯旋入殿。早期的 Janus 神像的两副面孔都有胡子，后来没有胡子，但是一副面孔年轻，另一副面孔年老。Janus 的右手指上刻有数字 CCC（300），左手指上刻着数字 LXV（65），合在一起恰是一年的天数。从纪元前 1 世纪起，罗马人把祭祀 Janus 的节日和新年结合在一起。罗马的执政官也在元旦这一天就职，并向 Janus 献祭，祈求国家的安宁。为了纪念 Janus，罗马人把正月称为 Januarius（mensis），意含“Janus 之月”，英文借用了该词，先作 Januarie，后作 January 。而在英文吸收 January 一词之前，撒克逊人把正月叫作 Wulf-Monath（wolf-month），意为“狼月”，因为此时正值严冬，是狼群出没村子寻觅食物的时节。

----------

> **关于 hotwheels（即风火轮）的历史**

>  双轮暗藏风火之势，行动间有风雷之声，故称风火轮。可踏在脚下作为交通工具，踏上其轮，念动咒语，上天入地，无所不能。 传说是青鸾火凤所化，一蹬九万里，双足十八万里。传说中为哪吒兵器之一。 

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

其中 

- **topman** - 维护 pubsub 进程和 Topic 的映射关系； 
- **pubsub** - 关联特定  Topic 的进程 ；维护所有订阅到该 Topic 的进程信息； 
- **janus_acceptor** - 处理来自网络的 TCP 连接；动态创建 transport 和 client_proxy 进程，以处理后续协议交互； 
- **transport** - 针对某个 TCP 连接上的数据处理； 
- **client_proxy** - 实际处理订阅，取消订阅，以及消息推送的模块； 
- **mapper** -  提供轻量级进程注册管理功能； 

----------

## subscriber 行为

```sequence
title: subscriber 行为
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
title: subscribe 的内部处理
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

----------

## publisher 行为

```sequence
title: publisher 行为
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
title: publish 的内部处理
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

----------

## 协议细节

### subscriber 协议交互

```
flashbot                                               janus

        <regular-socket/>,0,PING,0
		---------------------------------------------->
		
		{
			"timestamp":[1448,434925,303633],
			"token":[55,97,55,100,48,102,56,98,102,97,97,49,54,101,48,48,48,100,101,99,54,55,48,57,55,99,101,99,97,99,56,56]
		},1
		<----------------------------------------------
		
		<regular-socket/>,0,
		{
			"action":"subscribe",
			"data":"events"
		},0
		---------------------------------------------->
		
												ACK,1
		<----------------------------------------------
		
		{
			"timestamp":[1448,434925,395864],
			"topic":"events",
			"event":"test_event",
			"message_id":"",
			"data":"test"
		},1
		<----------------------------------------------
		
		<regular-socket/>,0,
		{
			"action":"unsubscribe",
			"data":"events"
		},0
		---------------------------------------------->
```

对应网络数据

```
00000000  3c 72 65 67 75 6c 61 72  2d 73 6f 63 6b 65 74 2f <regular -socket/
00000010  3e 00 50 49 4e 47 00                             >.PING.
    00000000  7b 22 74 69 6d 65 73 74  61 6d 70 22 3a 5b 31 34 {"timest amp":[14
    00000010  34 38 2c 34 33 34 39 32  35 2c 33 30 33 36 33 33 48,43492 5,303633
    00000020  5d 2c 22 74 6f 6b 65 6e  22 3a 5b 35 35 2c 39 37 ],"token ":[55,97
    00000030  2c 35 35 2c 31 30 30 2c  34 38 2c 31 30 32 2c 35 ,55,100, 48,102,5
    00000040  36 2c 39 38 2c 31 30 32  2c 39 37 2c 39 37 2c 34 6,98,102 ,97,97,4
    00000050  39 2c 35 34 2c 31 30 31  2c 34 38 2c 34 38 2c 34 9,54,101 ,48,48,4
    00000060  38 2c 31 30 30 2c 31 30  31 2c 39 39 2c 35 34 2c 8,100,10 1,99,54,
    00000070  35 35 2c 34 38 2c 35 37  2c 35 35 2c 39 39 2c 31 55,48,57 ,55,99,1
    00000080  30 31 2c 39 39 2c 39 37  2c 39 39 2c 35 36 2c 35 01,99,97 ,99,56,5
    00000090  36 5d 7d 01                                      6]}.
00000017  3c 72 65 67 75 6c 61 72  2d 73 6f 63 6b 65 74 2f <regular -socket/
00000027  3e 00 7b 22 61 63 74 69  6f 6e 22 3a 22 73 75 62 >.{"acti on":"sub
00000037  73 63 72 69 62 65 22 2c  22 64 61 74 61 22 3a 22 scribe", "data":"
00000047  65 76 65 6e 74 73 22 7d  00                      events"} .
    00000094  41 43 4b 01                                      ACK.
    00000098  7b 22 74 69 6d 65 73 74  61 6d 70 22 3a 5b 31 34 {"timest amp":[14
    000000A8  34 38 2c 34 33 34 39 32  35 2c 33 39 35 38 36 34 48,43492 5,395864
    000000B8  5d 2c 22 74 6f 70 69 63  22 3a 22 65 76 65 6e 74 ],"topic ":"event
    000000C8  73 22 2c 22 65 76 65 6e  74 22 3a 22 74 65 73 74 s","even t":"test
    000000D8  5f 65 76 65 6e 74 22 2c  22 6d 65 73 73 61 67 65 _event", "message
    000000E8  5f 69 64 22 3a 22 22 2c  22 64 61 74 61 22 3a 22 _id":"", "data":"
    000000F8  74 65 73 74 22 7d 01                             test"}.
00000050  3c 72 65 67 75 6c 61 72  2d 73 6f 63 6b 65 74 2f <regular -socket/
00000060  3e 00 7b 22 61 63 74 69  6f 6e 22 3a 22 75 6e 73 >.{"acti on":"uns
00000070  75 62 73 63 72 69 62 65  22 2c 22 64 61 74 61 22 ubscribe ","data"
00000080  3a 22 65 76 65 6e 74 73  22 7d 00                :"events "}.
```

### publisher 协议交互

```
flashbot                                               janus

        <regular-socket/>,0,PUBLISH,0
		{
			"topic":"events",
			"event":"test_event",
			"message_id":"",
			"data":"test"
		}
		---------------------------------------------->
		
		{
			"timestamp":[1448,434925,395595],
			"token":[100,52,97,53,56,51,54,99,57,97,50,52,50,57,52,57,55,48,52,102,48,100,99,53,102,55,56,101,101,97,53,98]
		},1
		<----------------------------------------------
```

对应网络数据

```
00000000  3c 72 65 67 75 6c 61 72  2d 73 6f 63 6b 65 74 2f <regular -socket/
00000010  3e 00 50 55 42 4c 49 53  48 00 7b 22 74 6f 70 69 >.PUBLIS H.{"topi
00000020  63 22 3a 22 65 76 65 6e  74 73 22 2c 22 65 76 65 c":"even ts","eve
00000030  6e 74 22 3a 22 74 65 73  74 5f 65 76 65 6e 74 22 nt":"tes t_event"
00000040  2c 22 6d 65 73 73 61 67  65 5f 69 64 22 3a 22 22 ,"messag e_id":""
00000050  2c 22 64 61 74 61 22 3a  22 74 65 73 74 22 7d    ,"data": "test"}
    00000000  7b 22 74 69 6d 65 73 74  61 6d 70 22 3a 5b 31 34 {"timest amp":[14
    00000010  34 38 2c 34 33 34 39 32  35 2c 33 39 35 35 39 35 48,43492 5,395595
    00000020  5d 2c 22 74 6f 6b 65 6e  22 3a 5b 31 30 30 2c 35 ],"token ":[100,5
    00000030  32 2c 39 37 2c 35 33 2c  35 36 2c 35 31 2c 35 34 2,97,53, 56,51,54
    00000040  2c 39 39 2c 35 37 2c 39  37 2c 35 30 2c 35 32 2c ,99,57,9 7,50,52,
    00000050  35 30 2c 35 37 2c 35 32  2c 35 37 2c 35 35 2c 34 50,57,52 ,57,55,4
    00000060  38 2c 35 32 2c 31 30 32  2c 34 38 2c 31 30 30 2c 8,52,102 ,48,100,
    00000070  39 39 2c 35 33 2c 31 30  32 2c 35 35 2c 35 36 2c 99,53,10 2,55,56,
    00000080  31 30 31 2c 31 30 31 2c  39 37 2c 35 33 2c 39 38 101,101, 97,53,98
    00000090  5d 7d 01                                         ]}.
```



