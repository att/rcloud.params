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

    function input_id(name) {
        return 'rcloud-params-' + name;
    }

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
        add_edit_control: function(context_id, desc, name, value, callback, k) {
            var input = $('<input type="text" id="#' + input_id(name) + '"></input>');
            var label = $('<label>' + desc + '</label>').append(input);
            input.val(value);
            input.change(function(val) {
                callback(input.val());
            });
            RCloud.session.selection_out(context_id, label);
            k(null, 1);
        },
        wait_submit: function(context_id, callback, k) {
            var submit = $('<input type="button" value="Submit" />');
            submit.click(function() {
                callback(function(err, res) {
                    if(res)
                        k();
                });
            });
            RCloud.session.selection_out(context_id, submit);
        },
        focus: function(name, k) {
            $('#' + input_id(name)).focus();
            k(null, 1);
        }
    };
})()) /*jshint -W033 */ // this is an expression not a statement
