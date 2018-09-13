library(bitcoinFinance)
cryptowatch_data_download("coinbase","btcusd","trades",
                query_options="limit=100",data_dir="D:/",logfile="D:/loader.log",
                register="D:/register.csv")
cryptowatch_data_download("coinbase","btcusd", "orderbook", data_dir="D:/",
                logfile="D:/loader.log", register="D:/register.csv")

