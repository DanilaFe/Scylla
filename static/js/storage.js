function setupStorage(app) {
    app.ports.setStoreValuePort.subscribe(function(data) {
        key = data[0];
        value = data[1];
        localStorage.setItem(key, value);
    });
    app.ports.getStoreValuePort.subscribe(function(data) {
        app.ports.receiveStoreValuePort.send({ "key" : data, "value" : localStorage.getItem(data) });
    });
}
