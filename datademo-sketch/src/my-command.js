const sketch = require("sketch");
const { DataSupplier } = sketch;
const util = require("util");
const { tsvParse } = require("d3-dsv");

export function onStartup() {
  // To register the plugin, uncomment the relevant type:
  DataSupplier.registerDataSupplier("public.text", "datademo", "SupplyData");
  // DataSupplier.registerDataSupplier('public.image', 'datademo', 'SupplyData')
}

export function onShutdown() {
  // Deregister the plugin
  DataSupplier.deregisterDataSuppliers();
}

const getRandomInt = (min, max) =>
  Math.floor(Math.random() * (max - min)) + min;

const getData = url =>
  fetch(url)
    .then(response => response.text())
    .then(tsv => tsvParse(tsv));

const composeData = composeObj =>
  Promise.all(
    Object.entries(composeObj).map(([key, url]) =>
      getData(url).then(tsv => [key, tsv])
    )
  ).then(Object.fromEntries);

export function onSupplyData(context) {
  // https://docs.google.com/spreadsheets/d/1X3ncgb0wsFD__1e0WWERltom7O1yIwbs8XM_SdJQfz8/edit?usp=sharing
  composeData({
    Product:
      "https://docs.google.com/spreadsheets/d/1X3ncgb0wsFD__1e0WWERltom7O1yIwbs8XM_SdJQfz8/export?format=tsv"
  }).then(dataSources => {
    const items = util.toArray(context.data.items).map(sketch.fromNative);
    const dataKey = context.data.key;

    console.log("=======> onSupplyData");
    let symbolMaps = {};

    const data = items.map(
      ({ name, type, override, symbolInstance }, index) => {
        const [dataDomainName, key] = (
          name || override.affectedLayer.name
        ).split(".");

        if (!dataDomainName || !key) return "";
        if (!dataSources[dataDomainName]) return "";

        const dataDomain = dataSources[dataDomainName];

        const dataIndex =
          symbolMaps[symbolInstance?.id] ?? getRandomInt(0, dataDomain.length);

        if (type === "DataOverride") {
          symbolMaps[symbolInstance?.id] = dataIndex;
        }

        console.log("dataIndex -------->", symbolMaps, dataIndex);

        const dataItem = dataDomain[dataIndex];

        console.log(dataItem[key]);

        if (!dataItem[key]) return "";

        return dataItem[key];
      }
    );

    DataSupplier.supplyData(dataKey, data);
  });
}
