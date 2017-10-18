var _abarbu$speedread$Native_XML = function(){
    function just(x) { return _elm_lang$core$Maybe$Just(x) }
    var nothing = _elm_lang$core$Maybe$Nothing
    function list(array) { return _elm_lang$core$Native_List.fromArray(array) }
    function maybe(x) { if(x) { return just(x) } else { return nothing } }
    function parse(doc){
        try { return maybe($.parseXML(doc)) }
        catch(err) { console.log(err); return nothing } }
    function find(xml, xpathquery){
        try { return list($(xml).xpath(xpathquery).toArray()) }
        catch(err) { console.log(err); return list([]) } }
    function attribute(xml, attribute){
        try { return just( xml.attributes[attribute].value ) }
        catch(err) { console.log(err); return nothing } }
    function text(xml){
        try { return maybe(xml.textContent) }
        catch(err) { console.log(err); return nothing } }
    function serialize(xml){ return (new XMLSerializer()).serializeToString(xml) }
    function firstChild(xml) {
        try { return maybe(xml.firstChild) }
        catch(err) { console.log(err); return nothing } }
    function nextSibling(xml) {
        try { return maybe(xml.nextSibling) }
        catch(err) { console.log(err); return nothing } }
    function parent(xml) {
        try { return maybe(xml.parentNode) }
        catch(err) { console.log(err); return nothing } }
    function nodeType(xml) {
        try { return maybe(xml.nodeType) }
        catch(err) { console.log(err); return nothing } }
    function name(xml) {
        try { return maybe(xml.nodeName) }
        catch(err) { console.log(err); return nothing } }
    function wrapNode(xml) {
        var obj = {}
        Object.defineProperty(obj, "node", {
            enumerable: false,
            writable: true
        })
        obj.node = xml
        return obj
    }
    function unwrapNode(obj) {
        return obj.node
    }
    function coerce(x) { return x }
    return {
        parse: parse,
        find: F2(find),
        attribute: F2(attribute),
        serialize: serialize,
        text: text,
        firstChild: firstChild,
        nextSibling: nextSibling,
        parent: parent,
        nodeType: nodeType,
        name: name,
        wrapNode: wrapNode,
        unwrapNode: unwrapNode,
        coerce: coerce
    }
}();
