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

    var _varmap = {}, _backend, _disabled_callbacks = [];
    
    function get_input_value(control) { // takes jquery object and extracts value
         if (control.find('button').length > 0) {
           return undefined;
         }
         if (control.find('select').length > 0) {
            let selectedOpts = _.filter($(control).find('option'), (op) => { 
              return $(op).is(':selected') && !$(op).is(':disabled');
            });
            return _.map(selectedOpts, (op) => { 
                return $(op).val();
            });
          } else if (control.find('input[type="checkbox"]').length > 0) {
            return control.find('input[type="checkbox"]').is(':checked');	
          } else if (control.find('input[type="radio"]').length > 0) {
            let selectedOpts = _.filter($(control).find('input[type="radio"]'), (op) => { 
              return $(op).is(':checked') && !$(op).is(':disabled');
            });
            let values = _.map(selectedOpts, (op) => { 
                return $(op).val();
            });
            if (values.length > 0) {
              return values[0];
            } 
            return undefined;
          } else {
            return control.find('input').val().trim();
          }
    }
    
    function get_default_input_value(control) { // takes jquery object and extracts value
         if (control.find('button').length > 0) {
           return undefined;
         }
         if (control.find('select').length > 0) {
            let selectedOpts = _.filter($(control).find('option'), (op) => { 
              return $(op).is('[data-rcloud-params-default-value="true"]') && !$(op).is(':disabled');
            });
            return _.map(selectedOpts, (op) => { 
                return $(op).val();
            });
          } else if (control.find('input[type="checkbox"]').length > 0) {
            return control.find('input[type="checkbox"]').is('[data-rcloud-params-default-value="true"]');	
          } else if (control.find('input[type="radio"]').length > 0) {
            let selectedOpts = _.filter($(control).find('input[type="radio"]'), (op) => { 
              return $(op).is('[data-rcloud-params-default-value="true"]') && !$(op).is(':disabled');
            });
            let values = _.map(selectedOpts, (op) => { 
                return $(op).val();
            });
            if (values.length > 0) {
              return values[0];
            } 
            return undefined;
          } else {
            let defaultValue = control.find('input').data('rcloud-params-default-value');
            if (defaultValue !== null && defaultValue !== undefined) {
              if (typeof(defaultValue) === 'string') {
               return defaultValue.trim();
              } else {
                return defaultValue;
              }
            }
            return undefined;
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
    
    function invokeBackend(el, name, val, e) {
      let form = el.closest('form');
      if(form.length > 0) {
        if(_disabled_callbacks.indexOf(form.get(0).id) < 0) {
            _backend.handle_event(name, val, { type: e.type });
        }
      }
    }
    
    function disableCallbacksForForm(form_id) {
      _disabled_callbacks.push(form_id);
    }
    
    function enableCallbacksForForm(form_id) {
      let index = _disabled_callbacks.indexOf(form_id);
      if(index >= 0) {
        _disabled_callbacks.splice(index, 1);
      }
    }
    
    
    function set_query(key, value, defaultValue) {
      if (value !== undefined && value !== null && value !== '' && value != defaultValue) {
          _varmap[key] = value;
      } else {
          delete _varmap[key];
      }
      querystring.update(_varmap);
    }
    
    var result = {
        init: function (ocaps, k) {
          
          _backend = RCloud.promisify_paths(ocaps, [  // jshint ignore:line
                    ['handle_event']
                ], true);
          
          let attachCallbacks = (n) => {
                let el = $(n);
                
                if(!el.is('form') && el.find('button').length === 0) {
                  
                  let requiredValidationRule = (control) => {
                        if (!isValueProvided(control)) {
                          control.addClass('has-error');
                        } else {
                          control.removeClass('has-error');
                        }
                  };
                
                  requiredValidationRule(el);
                
                  let input = el.find('select, input');
                  
                  let inputValue = get_input_value(el);
                  let defaultValue = get_default_input_value(el);
                  let name = el.data('rcloud-params-name');
                  
                  set_query(name, inputValue, defaultValue);
                  
                  input.on('change', function(e) {
                      requiredValidationRule(el);
                  });
                  
                  el.on('change', function (e) {
                      let val = get_input_value(el);
                      let input = el.find('select, input');
                      let defaultValue = get_default_input_value(el);
                      let name = el.data('rcloud-params-name');
      
                      if (val === '') val = undefined;
                      set_query(name, val, defaultValue);
                      invokeBackend(el, name, val, e);
                  });
                } else if(el.find('button[type="button"]').length > 0) {
                    _.forEach(el.find('button[type="button"]'), (b) => {
                      let $b = $(b);
                      $b.on('click', function (e) {
                          let val = true;
                          let name = el.data('rcloud-params-name');
                          invokeBackend(el, name, val, e);
                      });
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
        
        wait_for_form: function (context_id, form_id, k) {
            disableCallbacksForForm(form_id);
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let form = $('form[name="'+ form_id + '"]');
              if(form.length > 0) {
                form.submit(function (e) {
                  try {
                    let invalidControls = form.find('.has-error');
    
                    if (invalidControls.length === 0) {
                        form.find('button[type="submit"]').attr('disabled', 'disabled');
                        let controls = form.find('input, select');
                        let values = _.map(controls, (inputTag) => {
                          let $inputTag = $(inputTag);
                          return {
                            name: $inputTag.data('rcloud-params-name'),
                            value: get_input_value($inputTag.parent())
                          };
                        });
                        k(null, values);
                        enableCallbacksForForm(form_id);
                    } else {
                        $(invalidControls[0]).focus();
                    }

                  } catch (err) {
                    console.error(err);
                    k(err, null);
                    enableCallbacksForForm(form_id);
                  } finally {
                    return false; // Never submit the form, it would refresh the edit screen
                  }
                });
              }
              
          });
        },
        
        run_cell: function(cell_id, k) {
          let matching_cells = _.filter(shell.notebook.model.cells, (c) => { 
            return c.id() == cell_id; 
          });
          if (matching_cells.length > 0) {
            shell.run_notebook_cells([cell_id]);
          } else {
            console.error("Cell with id " + cell_id + " not found!");
          }
          k();
        },
        
        run_cells: function(cell_ids, k) {
          try {
            let matching_cells = _.filter(shell.notebook.model.cells, (c) => { 
              return cell_ids.indexOf(c.id()) >= 0; 
            });
            if (matching_cells.length > 0) {
              shell.run_notebook_cells(cell_ids);
            } else {
              console.error("Cells with ids " + cell_ids + " not found!");
            }
          } finally {
            k();
          }
        },
        
        run_cells_from: function(cell_id, k) {
          try {
            let matching_cells = _.filter(shell.notebook.model.cells, (c) => { 
              return c.id() == cell_id; 
            });
            if (matching_cells.length > 0) {
                shell.run_notebook_from(cell_id);
            } else {
              console.error("Cell with id " + cell_id + " not found!");
            }
          } finally {
            k();
          }
        },
        
        stop_execution: function(k) {
          try {
            RCloud.UI.processing_queue.stopGracefully()
          } finally {
            k();
          }
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
