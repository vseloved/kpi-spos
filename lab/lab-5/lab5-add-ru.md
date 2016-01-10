## Цель

Цель этой работы — ознакомиться с тем, как создавать и использовать модули ядра ОС Linux.

В результате ее выполнения произойдет овладение концепциями, связанными с реализацией модулей ядра ОС Linux.


## Задание

Написать на языке С модуль ОС Linux, который реализует то же задание, что и в лабораторной работе №5 (в этот раз без использования FUSE).


## Разработка модулей ядра ОС Linux

Простейший драйвер "hello world" имеет следующие вид:

    #include <linux/init.h>
    #include <linux/module.h>
    
    MODULE_LICENSE("Dual BSD/GPL");
    
    static int hello_init(void)
    {
            printk(KERN_ALERT "Hello, world\n");
            return 0;
    }
    
    static void hello_exit(void)
    {
            printk(KERN_ALERT "Goodbye, cruel world\n");
    } 
    
    module_init(hello_init);
    module_exit(hello_exit);

В этой программе макросы `modulde_init` и `module_exit` определяют, какие функции являются точкой входа и выхода из модуля.

В модуле ядра нет доступа к стандартным потокам ввода-вывода, поэтому печать может осуществляться только в системный лог-файл с помощью функции `printk`. Результат этого вывода можно увидеть с помощью команды `dmesg`.

Кроме того, в коде модуля нет доступа к функциям стандартной библиотеки С, такими как, например, `printf`. Вместо них в ядре реализована собственная "стандартная" библиотека — [Linux Kernel API](https://www.kernel.org/doc/htmldocs/kernel-api/).

Для сборки модуля "hello world" необходимо создать следующий `Makefile`:

    obj-m := hello.o
    
    KDIR := /lib/modules/<версия ядра>/build
    
    PWD := $(shell pwd)
    
    default: $(MAKE) -C $(KDIR) M=$(PWD) modules

Текущую версию ядра ОС можно узнать с помощью команды `uname -r`:

    $ uname -r
    3.11.0-12-generic

В случае отсутствия необходимых заголовочных файлов их можно установить с помощью менеджера пакетов ОС. Например, в Debian-подобных Linux системах это можно сделать следующим образом:

    $ sudo apt-get install build-essential linux-headers-$(uname -r)

Для загрузки собранного модуля используется команда `insmod`. Для выгрузки - `rmmod`.

    $ sudo insmod hello.ko
    $ dmesg | tail -1
    [ 8394.731865] Hello, world
    $ sudo rmmod hello.ko
    $ dmesg | tail -1
    [ 8707.989819] Goodbye, cruel world


## Литература

- [How to Write Your Own Linux Kernel Module with a Simple Example](http://www.thegeekstuff.com/2013/07/write-linux-kernel-module/)
- [Linux Device Drivers](http://lwn.net/Kernel/LDD3/)
- [The Linux Kernel Module Programming Guide](http://www.tldp.org/LDP/lkmpg/2.6/html/lkmpg.html)
- [Linux kernel and driver development training](http://free-electrons.com/doc/training/linux-kernel/linux-kernel-labs.pdf)
- [Как начать писать под ядро Linux (видео)](https://www.youtube.com/watch?v=m5Bgh5qyTI4)
