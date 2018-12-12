if("Notification" in window) {
    Notification.requestPermission();
}

function setupNotificationPorts(app) {
    app.ports.sendNotificationPort.subscribe(function(data) {
        var options = {
            "body" : data.text
        }
        new Notification(data.name, options)
    })
}
