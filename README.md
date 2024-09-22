# 文件分割与邮件发送工具

这是一个 Haskell 项目，用于将大文件分割成小块，并通过电子邮件发送这些文件块。

## 模块设计和作用

### 1. Main (app/Main.hs)

主模块，包含程序的入口点。它协调其他模块的功能，实现了文件压缩、分割和发送的主要逻辑。

主要功能：
- 压缩文件（如果需要）
- 根据文件大小决定是否需要分割
- 调用其他模块进行文件分割和邮件发送

### 2. Split (app/Split.hs)

负责文件分割的模块。

主要功能：
- 定义数据单位（B, KB, MB, GB, TB）
- 创建分割块（Chunk）
- 将文件分割成指定大小的块

### 3. Smtp (app/Smtp.hs)

处理电子邮件发送的模块。

主要功能：
- 定义 SMTP 配置
- 创建带有附件的邮件
- 发送单个或多个文件作为邮件附件

### 4. Zip (app/Zip.hs)

处理文件压缩的模块。

主要功能：
- 支持不同的压缩工具（7-Zip 和 ZIP）
- 生成压缩命令
- 执行文件压缩

### 5. Utils (app/Utils.hs)

提供各种实用函数的模块。

主要功能：
- 操作系统检测
- 文件类型检查
- 进程执行
- 其他辅助函数

### 6. ContentType (app/ContentType.hs)

定义文件内容类型的模块（文件中未显示，但在 Smtp 模块中被引用）。

## 使用方式

1. 确保已安装 Haskell 和 Cabal。

2. 克隆项目并进入项目目录：
   ```
   git clone <项目地址>
   cd sender
   ```

3. 编译项目：
   ```
   cabal build
   ```

4. 配置 SMTP 设置：
   编辑 `config/smtpConfig.yaml` 文件，填入您的 SMTP 服务器信息。

5. 运行程序：
   ```
   cabal run sender
   ```

6. 程序将压缩指定的文件（如果需要），根据大小决定是否分割，然后通过邮件发送。

## 注意事项

- 确保 SMTP 配置正确，否则邮件发送将失败。
- 大文件会被自动分割，每个分割后的文件大小默认为 20MB。
- 程序默认使用 7-Zip 进行压缩（在 Windows 上），其他系统使用 ZIP。请确保已安装相应的压缩工具。

## 配置文件

SMTP 配置文件 (`config/smtpConfig.yaml`) 示例：

```yaml
smtpHost: "smtp.example.com"
smtpPort: 587
smtpUser: "your_email@example.com"
smtpPassword: "your_password_or_app_password"
```

请根据您的实际 SMTP 服务器设置修改这些值。
