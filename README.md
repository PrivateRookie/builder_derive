# builder_derive
Rust Proc Macro Workshop Builder 宏实现

参考: https://github.com/jonhoo/proc-macro-workshop

## Usage

添加依赖
```toml
builder_derive = { git = "https://github.com/PrivateRookie/builder_derive.git" }
```

使用

```rust
use builder_derive::Builder;

#[derive(Builder)]
struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    env: Option<Vec<String>>,
    current_dir: Option<String>,
}

let mut builder: CommandBuilder = Command::builder();
builder.executable("cargo".to_string());
builder.env(vec!["PATH=/home/rookie/work".to_string()]);
// 相比于 env 一次性传入所有参数，使用 #[builder(each = "xx")]
// 可以每次push一个元素，多次调用
builder.arg("run".to_string());
builder.arg("--release".to_string());
builder.current_dir("/home/rookie/space".to_string());
let command = builder.build();
```

## 实现说明

可以直接查看代码 [src/lib.rs](./src/lib.rs) 里面写了一些注释

也可以查看 [how_to_proc](https://privaterookie.github.io/how_to_proc/)

