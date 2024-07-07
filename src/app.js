var wasm;
var dagre_render;

const wasm_imports = {
    env: {
        console_print: (lvl, addr, len) => {
            const msg = new TextDecoder()
                .decode(new Uint8Array(wasm.instance.exports.memory.buffer, addr, len));
            if (lvl === 3) {
                console.error("WASM: " + msg);
            } else if (lvl === 2) {
                console.warn("WASM: " + msg);
            } else if (lvl === 1) {
                console.info("WASM: " + msg);
            } else if (lvl === 0) {
                console.debug("WASM: " + msg);
            } else {
                console.log("WASM: " + msg);
            }
        }
    },
};



window.onload = async function() {
    d3.select("svg").call(d3.zoom().on("zoom", function() {
        d3.select("svg g").attr("transform", d3.event.transform);
    }));

    dagre_render = dagreD3.render();

    const wasm_response = await fetch("bin/ReFsm.wasm");
    const wasm_bytes = await wasm_response.arrayBuffer();
    wasm = await WebAssembly.instantiate(wasm_bytes, wasm_imports);
};

const input = document.querySelector("input");

function onUserInput() {
    const {
        memory: wasm_memory,
        alloc_input_regex,
        generate_digraph,
        get_digraph_addr,
        get_digraph_length,
    } = wasm.instance.exports;


    const input_regex = input.value;
    const input_regex_addr = alloc_input_regex(input_regex.length);
    console.log(input_regex, input_regex_addr);

    if (0 <= input_regex_addr &&
        input_regex_addr + input_regex.length <= wasm_memory.buffer.byteLength) {
        new Uint8Array(wasm_memory.buffer, input_regex_addr, input_regex.length)
            .set(new TextEncoder().encode(input_regex));
    }

    if (generate_digraph() == 0) {
        input.classList.add("error");
        return;
    } else{
        input.classList.remove("error");
    }

    const dot_digraph = new TextDecoder()
        .decode(new Uint8Array(wasm_memory.buffer, get_digraph_addr(), get_digraph_length()));

    const g = graphlibDot.read(dot_digraph);
    // Set margins, if not present
    if (!g.graph().hasOwnProperty("marginx") &&
        !g.graph().hasOwnProperty("marginy")) {
        g.graph().marginx = 20;
        g.graph().marginy = 20;
    }

    g.graph().transition = (selection) =>
        selection.transition().duration(500);

    // Render the graph into svg g
    d3.select("svg g").call(dagre_render, g);
}


input.addEventListener("keyup", onUserInput);
