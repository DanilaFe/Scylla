function setupNotificationPorts(app) {
    app.ports.requestMarkdownPort.subscribe(function(data) {
        app.ports.receiveMarkdownPort.send({
            "roomId" : data.roomId,
            "text" : data.text,
            "markdown" : marked(data.text)
        });
    })
}
