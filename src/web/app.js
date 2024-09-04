const wasm_console_logger = {
    message: "",
    write: function (ptr, len) {
        this.message += new TextDecoder("utf-8").decode(wasm.memory.buffer.slice(ptr, ptr + len));
    },
    flush: function (level) {
        if (level == 4) {
            console.error(this.message)
        } else if (level == 3) {
            console.warn(this.message)
        } else if (level == 2) {
            console.info(this.message)
        } else if (level == 1) {
            console.debug(this.message)
        } else {
            console.log(this.message)
        }
        this.message = ""
    }
}

function str_span(ptr) {
    const array = new Uint8Array(wasm.memory.buffer, ptr)
    const mem = array.slice(0, array.indexOf(0))
    return new TextDecoder("utf-8").decode(mem)
}

function set_input_regex(input_regex) {
    const encoded = new TextEncoder("utf-8").encode(input_regex)
    const input_regex_ptr = wasm.allocate_input_regex(encoded.byteLength)
    const input_regex_mem = new Uint8Array(wasm.memory.buffer, input_regex_ptr, encoded.byteLength)
    input_regex_mem.set(encoded)
}

const { instance: { exports: wasm } } = await WebAssembly.instantiateStreaming(await fetch("re-fsm.wasm"), {
    js: {
        console_log_write: wasm_console_logger.write.bind(wasm_console_logger),
        console_log_flush: wasm_console_logger.flush.bind(wasm_console_logger),
        panic: () => {
            throw new Error();
        },
    },
})

window.g = wasm
window.h = wasm_console_logger
window.i = set_input_regex
window.j = str_span
