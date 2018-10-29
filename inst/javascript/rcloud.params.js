((function () {
    // stolen from dc.graph.js, could be made its own tiny library
    // Used to retrive and update url
    var querystring = (function () {
        var _init_window_href = null;
        return {
            parse: function () {
                return (function (a) {
                    if (a == "") return {};
                    var b = {};
                    for (var i = 0; i < a.length; ++i) {
                        var p = a[i].split('=', 2);
                        if (p.length == 1)
                            b[p[0]] = "";
                        else
                            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
                    }
                    return b;
                })(window.location.search.substr(1).split('&'));
            },
            update: function (m) {
                var base = window.location.protocol + '//' + window.location.host + window.location.pathname;
                var parts = [];
                for (var k in m)
                    parts.push(k + '=' + encodeURIComponent(m[k]));
                var url = base + '?' + parts.join('&');
                if(!this._init_window_href) {
                   this._init_window_href = window.location.href;
                   window.history.pushState(null, null, url);
                 } else {
                   window.history.replaceState(null, null, url);
                 }
                 return this;
            }
        };
    })();

    var _varmap = {}, _varClass = {}, _defaults = {}, _needed = [], _element_fragments = {}, _backend;

    function input_id(name) {
        return 'rcloud-params-' + name;
    }

    function have_value(name) {
        return _varmap[name] !== undefined && _varmap[name] !== null && _varmap[name] !== '';
    }

    function get_input_value(control) { // takes jquery object and extracts value
         if(control.find('button').length > 0) {
           return undefined;
         }
         if (control.find('select').length > 0) {
            let selectedOpts = _.filter($(control).find('option'), (op) => { 
              return $(op).is(':selected');
            });
            return _.map(selectedOpts, (op) => { 
                return $(op).val();
            });
          } else if(control.find('input[type="checkbox"]').length > 0) {
            return control.find('input[type="checkbox"]').is(':checked');	
          } else {
            return control.find('input').val().trim();
          }
    }
    
    function get_element_fragment(control_descriptor) {
      if(!control_descriptor.id) {
        throw new Error('Expected control descriptor, but got ' + control_descriptor);
      }
      if(_element_fragments[control_descriptor.id]) {
          return _element_fragments[control_descriptor.id];
      } else {
          console.warn('Control with id "' + content + '" does not exist"');
      }
      return null;
    }
    
    function get_control_element(content) {
       if (content.hasOwnProperty('id')) {
          return  get_element_fragment(content);
        } else if (_.isFunction(content)) {
          let id = content();
          return $('#' + id);
        } else {
          return $('#' + content);
        }
    }
    // Schedules execution of a function within cell result processing loop to ensure that any UI element referenes used in the function
    // were added to the result pane.
    function executeInCellResultProcessingLoop(context_id, fun) {
      RCloud.session.invoke_context_callback('function_call', context_id, fun);
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
        init: function (ocaps, k) {
          
          _backend = RCloud.promisify_paths(ocaps, [  // jshint ignore:line
                    ['handle_event']
                ], true);
          
          let attachCallbacks = (n) => {
                let el = $(n);
                let inputValue = get_input_value(el);
                let name = el.data('rcloud-params-name');
                let varClass = el.data('rcloud-params-rclass');
                
                _needed.push(name);
                result.set_query(name, inputValue, varClass);
                
                let requiredValidationRule = (control) => {
                      let val = get_input_value(control);
                      if (val === null || val === undefined || val === '' || val.length === 0) val = undefined;
                      
                      if (val === undefined) {
                        control.addClass('has-error');
                      } else {
                        control.removeClass('has-error');
                      }
                };
                
                requiredValidationRule(el);
                
                if(!el.is('submit')) {
                  let input = el.find('select, input');
                  
                  input.on('change', function(e) {
                      requiredValidationRule(el);
                  });
                  
                  el.on('change', function (e) {
                      let val = get_input_value(el);
                      let name = el.data('rcloud-params-name');
      
                      if (val === '') val = undefined;
                      result.set_query(name, val, el.data('rcloud-params-rclass'));
                      _backend.handle_event(name, val, e);
                  });
                } else {
                  el.on('click', function (e) {
                      let val = get_input_value(el);
                      let name = el.data('rcloud-params-name');
                      _backend.handle_event(name, val, e);
                  });
                }
          };

          var observer = new MutationObserver(function(mutations) {
            _.forEach(mutations, (m) => { 
              _.forEach(m.addedNodes, (n) => {
                  if($(n).data('rcloud-params') && $(n).data('rcloud-params') === 'TRUE') {
                    attachCallbacks(n);
                  }
                  _.forEach($(n).find('[data-rcloud-params="TRUE"]'), attachCallbacks);
              });
            });
          });

          observer.observe(document, {attributes: false, childList: true, characterData: false, subtree:true});
          
          _varmap = querystring.parse();
          k(null, _varmap);
        },
        set_query: function (key, value, varClass, k) {
            if (value !== undefined && value !== null && value !== '') {
                _varmap[key] = value;
                _varClass[key] = varClass;
            } else {
                delete _varmap[key];
            }
            querystring.update(_.pick(_varmap, _.filter(Object.keys(_varmap), (k) => { 
              return _defaults[k] !== _varmap[k]; 
            })));
            if (k)
                k(null, 1);
        },
        focus: function (name, k) {
            $('#' + input_id(name)).focus();
            if (k)
                k(null, 1);
        },
        error_highlight: function(names, whether, k) {
            if (!_.isArray(names))
                names = [names];
            var sel = $(names.map(function (n) { return '#' + input_id(n); }).join(',')),
                border = whether ? 'red 1px solid' : '';
            sel.css('border', border);
            if (k)
                k(null, 1);
        },
        // copied over from rcloud.web - need to be moved back to caps.R       
        appendDiv: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              if (_.isFunction(content)) content = content();
              if (div) {
                $(div).append(content);
              } else {
                result_div.append(content);
              }
            });
            k(true);
        },
        appendElement: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
            let el = get_control_element(content);
            if(el) {
              if (div) {
                $(div).append(el);
              } else {
                result_div.append(el);
              }
            }
            });
            k(true);
        },
        prependDiv: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              if (_.isFunction(content)) content = content();
              if (div) {
                $(div).prepend(content);
              } else {
                result_div.prepend(content);
              }
            });
            k(true);
        },
        prependElement: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let el = get_control_element(content);
              if(el) {
                if (div) {
                  $(div).prepend(el);
                } else {
                  result_div.prepend(el);
                }
              }
            });
            k(true);
        },
        setDiv: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              if (_.isFunction(content)) content = content();
              if (div) {
                $(div).empty();
                $(div).append(content);
              } else {
                result_div.empty();
                result_div.append(content);
              }
            });
            k(true);
        },
        
        setElement: function (context_id, div, content, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let el = get_control_element(content);
              if(el) {
                if (div) {
                  $(div).empty();
                  $(div).append(el);
                } else {
                  result_div.empty();
                  result_div.append(el);
                }
              }
            });
            k(true);
        },

        add_edit_control: function (context_id, desc, name, defaultValue, value, inputTag, labelTag, varClass, callback, k) {
            var input = $(inputTag);
            var label = $(labelTag).append(input);
            
            _varClass[name] = varClass;
            
            let isNotEmpty = (v) => {
              return v !== undefined && v !== null;
            };
            
            if (isNotEmpty(defaultValue)) {
               _defaults[name] = defaultValue;
            }
            
            let inputVal;
            
            if (isNotEmpty(value)) {
              inputVal = value;
            } else if (isNotEmpty(defaultValue)) {
              inputVal = defaultValue;
            }

            input.val(inputVal);
            result.set_query(name, inputVal, varClass);

            label.on('change', function () {

                var val = get_input_value(label);

                if (val === '') val = undefined;
                result.set_query(name, val, varClass);
            });
           
            _needed.push(name);
            _element_fragments[label.attr('id')] = label;
            k(label.attr('id'));      
        },

        wait_submit: function (context_id, k) {
            var submit = $('<input id = "rcloud-params-submit" type="button" value="Submit" />');
            submit.click(function () {

                var good_bad = _.partition(_needed, have_value);

                result.error_highlight(good_bad[0], false);
                if (!good_bad[1].length) {
                    submit.attr('disabled', 'disabled');
                    var varValues = _.pick(_varmap, _needed);
                     _needed = [];  // clear _needed  only current variables need to be called back to r 
                    k(combine(varValues, _varClass));
                } else {
                    result.error_highlight(good_bad[1], true);
                    result.focus(good_bad[1][0]);
                }
            });
            RCloud.session.invoke_context_callback('selection_out', context_id, submit);
        },

        wait_for_group: function (context_id, group, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let el = $('label[data-rcloud-params-group="'+ group + '"]').find('input[type="submit"]');
              if(el.length > 0) {
                el.click(function (e) {
                  let controls = $('label[data-rcloud-params-group="'+ group + '"]');
                  var good_bad = _.partition(_needed, have_value);
  
                  result.error_highlight(good_bad[0], false);
                  if (!good_bad[1].length) {
                      el.attr('disabled', 'disabled');
                      var varValues = _.pick(_varmap, _needed);
                       _needed = [];  // clear _needed  only current variables need to be called back to r 
                      k(combine(varValues, _varClass));
                  } else {
                      result.error_highlight(good_bad[1], true);
                      result.focus(good_bad[1][0]);
                  }
                });
              } else {
                k('Group "' + group + '" does not have submitParam, please add one.', null);
              }
              
          });
        },
        
        log: function(content, k) {
            console.log(content);
            k();
        },

        debug: function(content, k) {
            console.debug(content);
            k();
        },
    };
    window.RCloud.params = { instance:  result };
    return result;
})()) /*jshint -W033 */ // this is an expression not a statement
