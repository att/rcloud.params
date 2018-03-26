((function() {

    // leave require empty for now - ui control libraries will go here
    requirejs.config({
        "paths": {
        },
        "shim": {
        }
    });

    // stolen from dc.graph.js, could be made its own tiny library
    // Used to retrive and update url
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

    var _varmap, _varClass = [], _defaults = {}, _needed = [];

    function input_id(name) {
        return 'rcloud-params-' + name;
    }
    function have_value(name) {
        return _varmap[name] != undefined ||
            _defaults[name] !== undefined;
    }
    function get_input_value(label){ // takes jquery object and extracts value
     
              if(label[0].childNodes[1].nodeName.toLowerCase() == 'select'){
                // if a select get all selected objects
                return $('#' + label[0].childNodes[1].id + ' option:selected').map(function() {return $(this).val();}).get();
              } else{
                return label[0].querySelector("[id^='rcloud-params-']").value.trim();
                
              }
    }
  // Used to combine value with class to push back to R  
  combine = (obj1, obj2) => {
	let returnObject = {},
  		objects = _.chain(_.map(Object.keys(obj1), (key) => {
  	return [obj1[key], obj2[key]];
  })).map((items, index) => {
  	return {
    	value: items[0],
      type: items[1]
    };
  }).value();

	_.each(Object.keys(obj1), (key, index) => {
  	returnObject[key] = objects[index];
  });
 
 	return returnObject;
};
    var result = {
        init: function(k) {
            _varmap = querystring.parse();
            k(null, _varmap);
        },
        set_query: function(key, value, varClass, k) {
            if(value !== undefined && _defaults[key] !== value) {
                _varmap[key] = value;
                _varClass[key] = varClass;
            }
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
  
        add_edit_control: function(context_id, desc, name, def, val, inputTag, labelTag, varClass, callback, k) {
   
            var input = $(inputTag);
            var label = $(labelTag).append(input);
            if(val !== null) {
                _varmap[name] = val ;
                _varClass[name] = varClass;
                input.val(val);
            }
            else if(def !== null) {
                _defaults[name] = def; 
                 _varClass[name] = varClass;
                input.val(def);
            }
            label.on('change', function() {
           
                var val = get_input_value(label);
                
                if(val === '') val = undefined;
                result.set_query(name, val, varClass);      
            });
            RCloud.session.invoke_context_callback('selection_out', context_id, label); 
            _needed.push(name);

          k(null, label.attr('id'));
           //k(null, label.html());
        },
        wait_submit: function(context_id, k) {
            var submit = $('<input id = "rcloud-params-submit" type="button" value="Submit" />');
            submit.click(function() {
             
                var good_bad = _.partition(_needed, have_value);
               
                result.error_highlight(good_bad[0], false);
                if(!good_bad[1].length) {
                    submit.attr('disabled', 'disabled');
                  var varValues = _.pick(_varmap, _needed);
                  k(combine(varValues, _varClass));
                }
                else {
                    result.error_highlight(good_bad[1], true);
                    result.focus(good_bad[1][0]);
                }
            });
            RCloud.session.invoke_context_callback('selection_out', context_id, submit);
        }
    };
    return result;
})()) /*jshint -W033 */ // this is an expression not a statement
