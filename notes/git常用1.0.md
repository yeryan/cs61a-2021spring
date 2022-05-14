ssh-ketgen -t rsa -C "账号"			生成ssh密钥

ssh -T git@github.com			试验配置是否成功

git config --global user.name "用户名"

git config --global user.email "邮箱"	

git config --global user.email "邮箱"

git init					将本地文件夹变为git可管理的仓库

git add . 					将项目添加到仓库

git commit -m "提交日志"

git remote add origin 粘贴地址		与仓库进行连接

git push -u origin master			上传

git pull --rebase origin master			内容合并

git remote add origin https://github.com/yeryan/yery.github.io.git

git branch -M main

git push -u origin main

git clone 粘贴地址				将fork的仓库同步到本地

git branch -m master main

git fetch origin

git branch -u origin/main main

git remote set-head origin -a
