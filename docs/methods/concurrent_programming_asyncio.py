import asyncio
import time

loop = asyncio.new_event_loop()


def delay(n, fun, *args):
    delay.count += 1
    loop.call_later(n, decr_and_call, fun, *args)


def decr_and_call(fun, *args):
    delay.count -= 1
    fun(*args)


delay.count = 0


def delay_until(prop, fun, *args):
    if prop():
        fun(*args)
    else:
        delay(1e-6, delay_until, prop, fun, *args)


## end of asyncio boilerplate

## begin of program / script


def f(x):
    f.count += 1
    t = time.time() - f.start
    print(f"at second {t:.0f}: {x}")


f.start = time.time()
f.count = 0

f("a")
delay(2, f, "d")
delay(0, f, "c")
delay(1, delay, 2, f, "f")
delay_until(lambda: f.count >= 4, f, "e")
f("b")

## end of program / script

## begin of asyncio boilerplate


async def main():
    while True:
        assert delay.count >= 0
        if delay.count == 0:
            return
        else:
            await asyncio.sleep(1e-6)


loop.run_until_complete(main())
