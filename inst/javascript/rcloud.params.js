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

    var _varmap = {}, _varClass = {}, _defaults = {}, _backend, _disabled_callbacks = [];
    
    function get_input_value(control) { // takes jquery object and extracts value
         if(control.find('button').length > 0) {
           return undefined;
         }
         if (control.find('select').length > 0) {
            let selectedOpts = _.filter($(control).find('option'), (op) => { 
              return $(op).is(':selected') && !$(op).is(':disabled');
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
    
    function get_control_element(content) {
      if (_.isFunction(content)) {
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
    
    function isValueProvided(control) {
      let val = get_input_value(control);
      if (val === null || val === undefined || val === '' || val.length === 0) val = undefined;
      if(val === undefined) {
        return false;
      }
      return true;
    }
    
    function invokeBackend(group, name, val, e) {
      if(_disabled_callbacks.indexOf(group) < 0) {
          _backend.handle_event(name, val, e);
      }
    }
    
    function disableCallbacksForGroup(group) {
      _disabled_callbacks.push(group);
    }
    
    function enableCallbacksForGroup(group) {
      let index = _disabled_callbacks.indexOf(group);
      if(index >= 0) {
        _disabled_callbacks.splice(index, 1);
      }
    }
    
    
    var result = {
        init: function (ocaps, k) {
          
          _backend = RCloud.promisify_paths(ocaps, [  // jshint ignore:line
                    ['handle_event']
                ], true);
          
          let attachCallbacks = (n) => {
                let el = $(n);
                
                if(!el.is('form') && el.find('button').length === 0) {
                  let inputValue = get_input_value(el);
                  let name = el.data('rcloud-params-name');
                  let varClass = el.data('rcloud-params-rclass');
                  
                  result.set_query(name, inputValue, varClass);
                  
                  let requiredValidationRule = (control) => {
                        if (!isValueProvided(control)) {
                          control.addClass('has-error');
                        } else {
                          control.removeClass('has-error');
                        }
                  };
                
                  requiredValidationRule(el);
                
                  let input = el.find('select, input');
                  
                  input.on('change', function(e) {
                      requiredValidationRule(el);
                  });
                  
                  el.on('change', function (e) {
                      let val = get_input_value(el);
                      let name = el.data('rcloud-params-name');
                      let group = el.data('rcloud-params-group');
      
                      if (val === '') val = undefined;
                      result.set_query(name, val, el.data('rcloud-params-rclass'));
                      invokeBackend(group, name, val, e);
                  });
                } else if(el.is('button')) {
                    el.on('click', function (e) {
                        let val = get_input_value(el);
                        let name = el.data('rcloud-params-name');
                        let group = el.data('rcloud-params-group');
                        invokeBackend(group, name, val, e);
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

        wait_for_group: function (context_id, group, k) {
            disableCallbacksForGroup(group);
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let el = $('form[data-rcloud-params-group="'+ group + '"]');
              if(el.length > 0) {
                el.submit(function (e) {
                  try {
                    let invalidControls = el.find('.has-error');
    
                    if (invalidControls.length === 0) {
                        el.find('submit').attr('disabled', 'disabled');
                        let controls = $('input[data-rcloud-params-group="'+ group + '"], select[data-rcloud-params-group="'+ group + '"]');
                        let values = _.map(controls, (inputTag) => {
                          let $inputTag = $(inputTag);
                          return {
                            r_class: $inputTag.data('rcloud-params-rclass'),
                            name: $inputTag.data('rcloud-params-name'),
                            value: get_input_value($inputTag.parent())
                          };
                        });
                        k(null, values);
                        enableCallbacksForGroup(group);
                    } else {
                        $(invalidControls[0]).focus();
                    }

                  } catch (err) {
                    console.error(err);
                    k(err, null);
                    enableCallbacksForGroup(group);
                  } finally {
                    return false; // Never submit the form, it would refresh the edit screen
                  }
                });
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
