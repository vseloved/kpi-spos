## Мета

Мета цієї роботи — ознайомитися з тим, як створювати і використовувати модулі ядра ОС Linux.

В результаті її виконання відбудеться оволодіння концепціями, пов'язаними з реалізацією модулів ядра ОС Linux.


## Завдання

Написати на мові С модуль ОС Linux, який реалізує те ж завдання, що і в лабораторній роботі №5 (на цей раз, без використання FUSE).


## Розробка модулів ядра ОС Linux

Найпростіший драйвер "hello world" має наступні вигляд:

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


У цій програмі макроси `modulde_init` і` module_exit` визначають, які функції є точкою входу і виходу з модуля.

У модулі ядра немає доступу до стандартних потоків введення-виведення, тож друк може здійснюватися тільки в системний лог-файл за допомогою функції `printk`. Результат цього виводу можна побачити за допомогою команди `dmesg`.

Крім того, в коді модуля немає доступу до функцій стандартної бібліотеки С, такими як, наприклад, `printf`. Замість них в ядрі реалізована власна "стандартна" бібліотека — [Linux Kernel API](https://www.kernel.org/doc/htmldocs/kernel-api/).

Для збиррання модуля "hello world" необхідно створити наступний `Makefile`:

    obj-m := hello.o
    
    KDIR := /lib/modules/<версия ядра>/build
    
    PWD := $(shell pwd)
    
    default: $(MAKE) -C $(KDIR) M=$(PWD) modules

Поточну версію ядра ОС можна дізнатися за допомогою команди `uname -r`:

    $ uname -r
    3.11.0-12-generic

У разі відсутності необхідних заголовкових файлів їх можна встановити за допомогою менеджера пакетів ОС. Наприклад, в Debian-подібних Linux системах це можна зробити наступним чином:

    $ sudo apt-get install build-essential linux-headers-$(uname -r)

Для завантаження зібраного модуля використовується команда `insmod`. Для вивантаження — `rmmod`.

    $ sudo insmod hello.ko
    $ dmesg | tail -1
    [ 8394.731865] Hello, world
    $ sudo rmmod hello.ko
    $ dmesg | tail -1
    [ 8707.989819] Goodbye, cruel world


## Література

- [How to Write Your Own Linux Kernel Module with a Simple Example] (http://www.thegeekstuff.com/2013/07/write-linux-kernel-module/)
- [Linux Device Drivers] (http://lwn.net/Kernel/LDD3/)
- [The Linux Kernel Module Programming Guide] (http://www.tldp.org/LDP/lkmpg/2.6/html/lkmpg.html)
- [Linux kernel and driver development training] (http://free-electrons.com/doc/training/linux-kernel/linux-kernel-labs.pdf)
- [Как начать писать под ядро Linux (відео)] (https://www.youtube.com/watch?v=m5Bgh5qyTI4)
