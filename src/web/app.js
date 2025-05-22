const dropdown_button = document.querySelector("#dropdown-button")
const dropdown_box = document.querySelector("#dropdown-box")
const output_box = document.querySelector("#output-box")
const form_flavor = document.querySelector("#form-flavor")
const optimize_mode = document.querySelector("#optimize-mode")
const regex_input = document.querySelector("#regex-input")
var flavors = {}

const console_logger = {
    message: "",
    write: function (ptr, len) {
        this.message += new TextDecoder("utf-8").decode(memory.buffer.slice(ptr, ptr + len))
    },
    flush: function (level) {
        if (level == 4) {
            console.error("[Wasm ERROR] " + this.message)
        } else if (level == 3) {
            console.warn("[Wasm WARN ] " + this.message)
        } else if (level == 2) {
            console.info("[Wasm INFO ] " + this.message)
        } else if (level == 1) {
            console.debug("[Wasm DEBUG] " + this.message)
        } else {
            console.log("[Wasm      ]" + this.message)
        }
        this.message = ""
    }
}

function str_span(ptr) {
    const array = new Uint8Array(memory.buffer, ptr)
    const mem = array.slice(0, array.indexOf(0))
    return new TextDecoder("utf-8").decode(mem)
}

const { instance: { exports: wasm } } = await WebAssembly.instantiateStreaming(await fetch("re-fsm.wasm"), {
    js: {
        console_log_write: console_logger.write.bind(console_logger),
        console_log_flush: console_logger.flush.bind(console_logger),
        panic: () => {
            throw new Error()
        },
    },
})
const memory = wasm.memory
const info = JSON.parse(str_span(wasm.info))

window.i = info

const viz = await Viz.instance();

var svg;

function update_regex_input() {
    if (svg != undefined) {
        output_box.removeChild(svg)
    }
    svg = undefined
    localStorage.setItem("regex-input", regex_input.value)
    const encoded = new TextEncoder("utf-8").encode(regex_input.value)
    const ptr = wasm.allocate_regex_input(encoded.byteLength)
    if (0 <= ptr && ptr + encoded.byteLength <= wasm.memory.buffer.byteLength) {
        const mem = new Uint8Array(memory.buffer, ptr, encoded.byteLength)
        mem.set(encoded)
    }
    if (!wasm.build_digraph() != 0) {
        if (regex_input.value.length != 0) {
            regex_input.classList.add("error")
        } else {
            regex_input.classList.remove("error")
        }
        return;
    } else {
        regex_input.classList.remove("error")
    }
    const digraph = str_span(wasm.render_digraph())
    svg = viz.renderSVGElement(digraph, {
        engine: "dot",
    })
    output_box.appendChild(svg)
    panzoom(svg)
}

function update_flavor() {
    const flavor = JSON.parse(form_flavor.value)
    localStorage.setItem("flavor", flavor.name)
    new Uint8Array(wasm.memory.buffer)[wasm.flavor.value] = flavor.value
    regex_input.placeholder = flavor.example
    update_regex_input()
}

optimize_mode.innerText = info.optimize

dropdown_button.addEventListener("click", function () {
    if (dropdown_box.classList.toggle("open")) {
        dropdown_button.innerText = 'x'
    } else {
        dropdown_button.innerText = '\u2630'
    }
})

form_flavor.innerHTML = ""
info.flavors.forEach(flavor => {
    const option = document.createElement("option")
    option.value = JSON.stringify(flavor)
    option.textContent = flavor.desc
    form_flavor.appendChild(option)
    flavors[flavor.value] = flavor
})

form_flavor.addEventListener("change", update_flavor)
regex_input.addEventListener("input", update_regex_input)

const stored_flavor = localStorage.getItem("flavor")
const flavor_found = info.flavors.find(flavor => flavor.name === stored_flavor) or "posix_bre"
if (flavor_found) {
    form_flavor.value = JSON.stringify(flavor_found)
    update_flavor()
}
const stored_regex_input = localStorage.getItem("regex-input")
if (stored_regex_input !== "") {
    regex_input.value = stored_regex_input
    update_regex_input()
}
