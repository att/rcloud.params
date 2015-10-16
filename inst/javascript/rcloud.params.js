((function() {

    // leave require empty for now - ui control libraries will go here
    requirejs.config({
        "paths": {
        },
        "shim": {
        }
    });

    // stolen from dc.graph.js, could be made its own tiny library
    var querystring = (function() {
        return {
            parse: function() {
                return (function(a) {
                    if (a == "") return {};
                    var b = {};
                    for (var i = 0; i < a.length; ++i)
                    {
                        var p=a[i].split('=', 2);
                        if (p.length == 1)
                            b[p[0]] = "";
                        else
                            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
                    }
                    return b;
                })(window.location.search.substr(1).split('&'));
            },
            update: function(m) {
                var base = window.location.protocol + '//' + window.location.host + window.location.pathname;
                var parts = [];
                for(var k in m)
                    parts.push(k + '=' + encodeURIComponent(m[k]));
                var url = base + '?' + parts.join('&');
                window.history.pushState(null, null, url);
                return this;
            }
        };
    })();

    var _varmap;

    return {
        init: function(k) {
            _varmap = querystring.parse();
            k(_varmap);
        },
        set_query: function(key, value, k) {
            _varmap[key] = value;
            querystring.update(_varmap);
            k(null, 1);
        },
        add_edit_control: function(context_id, label, k) {
            var input = $('<label type="text">' + label + '<input></input></label>');
            input.change(function(val) {
                alert(input.val());
            });
            RCloud.session.selection_out(context_id, input);
            k();
        }
    };
})()) /*jshint -W033 */ // this is an expression not a statement
