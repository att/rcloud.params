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

    var _varmap, _defaults = {}, _needed = [];

    function input_id(name) {
        return 'rcloud-params-' + name;
    }
    function have_value(name) {
        return _varmap[name] != undefined ||
            _defaults[name] !== undefined;
    }
    var result = {
        init: function(k) {
            _varmap = querystring.parse();
            k(null, _varmap);
        },
        set_query: function(key, value, k) {
            if(value !== undefined && _defaults[key] !== value)
                _varmap[key] = value;
            else
                delete _varmap[key];
            querystring.update(_varmap);
            if(k)
                k(null, 1);
        },
        focus: function(name, k) {
            $('#' + input_id(name)).focus();
            if(k)
                k(null, 1);
        },
        error_highlight(names, whether, k) {
            if(!_.isArray(names))
                names = [names];
            var sel = $(names.map(function(n) { return '#' + input_id(n); }).join(',')),
                border = whether ? 'red 1px solid' : '';
            sel.css('border', border);
            if(k)
                k(null, 1);
        },
        add_edit_control: function(context_id, desc, name, def, val, callback, k) {
            var input = $('<input type="text" id="' + input_id(name) + '"></input>');
            var label = $('<label>' + desc + '</label>').append(input);
            if(val !== null) {
                _varmap[name] = val;
                input.val(val);
            }
            else if(def !== null) {
                _defaults[name] = def;
                input.val(def);
            }
            input.change(function() {
                var val = input.val().trim();
                if(val === '') val = undefined;
                result.set_query(name, val);
            });
            RCloud.session.selection_out(context_id, label);
            _needed.push(name);
            k(null, 1);
        },
        wait_submit: function(context_id, k) {
            var submit = $('<input type="button" value="Submit" />');
            submit.click(function() {
                var good_bad = _.partition(_needed, have_value);
                result.error_highlight(good_bad[0], false);
                if(!good_bad[1].length)
                    k(_.pick(_varmap, _needed));
                else {
                    result.error_highlight(good_bad[1], true);
                    result.focus(good_bad[1][0]);
                }
            });
            RCloud.session.selection_out(context_id, submit);
        }
    };
    return result;
})()) /*jshint -W033 */ // this is an expression not a statement
