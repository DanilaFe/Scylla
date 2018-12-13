if("Notification" in window) {
    Notification.requestPermission();
}

function setupNotificationPorts(app) {
    app.ports.sendNotificationPort.subscribe(function(data) {
        var options = {
            "body" : data.text
        }
        var n = new Notification(data.name, options)
        n.onclick = function() {
            app.ports.onNotificationClickPort.send(data.room);
            n.close();
        }
    })
}
