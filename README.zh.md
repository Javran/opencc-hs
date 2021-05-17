# opencc-hs

[中文版](./README.zh.md)

這個庫包含 OpenCC 的高階和低階綁定。本庫是 Haskell FFI 的實驗，並不適合工業使用（至少現在不適合）。但是本庫非常簡單，您可以自行檢閱代碼並做出決斷。

## 比較

[hopencc](https://hackage.haskell.org/package/hopencc) 是一個現存的綁定庫，但是該庫僅提供了本庫中的 IO 接口。hopencc 會複製數據，而 opencc-hs 利用 bytestring 的內部表示，不需要複製。
