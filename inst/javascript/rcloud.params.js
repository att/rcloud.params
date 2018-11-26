((function () {

    var _varmap = {}, _backend, _disabled_callbacks = [];
    
    function getInputValue(control) { // takes jquery object and extracts value
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
    
    function getDefaultInputValue(control) { // takes jquery object and extracts value
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
    
    function isNotMini() {
      return window.editor && window.shell;
    }
    
    // Schedules execution of a function within cell result processing loop to ensure that any UI element referenes used in the function
    // were added to the result pane.
    function executeInCellResultProcessingLoop(context_id, fun) {
      if(isNotMini()) {
        RCloud.session.invoke_context_callback('function_call', context_id, fun);
      } else {
        fun.apply(undefined);
      }
    }
    
    function isValueProvided(control) {
      let val = getInputValue(control);
      if (val === null || val === undefined || val === '' || val.length === 0) val = undefined;
      if (val === undefined) {
        return false;
      }
      return true;
    }
    
    function invokeBackend(el, name, val, e) {
      let form = (!el.is('form')) ? el.closest('form'): el;
      if (form.length > 0) {
        if (_disabled_callbacks.indexOf(form.get(0).id) < 0) {
            _backend.handle_event(name, val, { type: e.type });
        }
      } else {
        _backend.handle_event(name, val, { type: e.type });
      }
    }
    
    function disableCallbacksForForm(form_id) {
      _disabled_callbacks.push(form_id);
    }
    
    function enableCallbacksForForm(form_id) {
      let index = _disabled_callbacks.indexOf(form_id);
      if (index >= 0) {
        _disabled_callbacks.splice(index, 1);
      }
    }
    
    function setUrlQuery(key, value, defaultValue) {
      if (value !== undefined && value !== null && value !== '' && !_.isEqual(value, defaultValue) && value != defaultValue) {
          _varmap[key] = value;
      } else {
          _varmap[key] = undefined;
      }
      url_utils.updateHistory(_varmap, {baseUrl: url_utils.getBase(), segments: url_utils.getPathSegments() });
    }
    
    
    function validateControl(control) {
      
      let required = _.all(control.find('select, input'), (c) => { return $(c).is(':required');});
      
      if (required && !isValueProvided(control)) {
        control.addClass('has-error');
      } else {
        control.removeClass('has-error');
      }
    }
    
    function validateForm(form) {
      _.forEach(form.find('[data-rcloud-params="TRUE"]'), (el) => {
            let control = $(el);
            if (control.find('button').length === 0) {
                validateControl(control);
            }
      });
    }
    
    function isSubmitEventBound(form) {
      let events = jQuery._data( form.get(0), "events" );
      var data = (events)? events.submit : undefined;
       if (data === undefined || data.length === 0) {
        return false;
      }
      return true;
    }
    
    function onFormSubmit(form, callback, onError) {
        return function(e) {
          try {
              let invalidControls = form.find('.has-error');
              if (invalidControls.length === 0) {
                  let controls = _.filter(form.find('div[data-rcloud-params="TRUE"]'), (c) => { return $(c).find('button').length === 0; });
                  let values = _.map(controls, (control) => {
                  let $control = $(control);
                      return {
                        name: $control.data('rcloud-params-name'),
                        value: getInputValue($control)
                      };
                  });
                  if(callback) {
                    callback(form, values);
                  }
                } else {
                  $(invalidControls[0]).focus();
                }
  
            } catch (err) {
                console.error(err);
                if(onError) {
                  onError(form, err);
                }
            } finally {
                return false; // Never submit the form, it would refresh the edit screen
            }
        };
    }
    
    var result = {
        init: function (ocaps, k) {
          
          _backend = RCloud.promisify_paths(ocaps, [  // jshint ignore:line
                    ['handle_event'],
                    ['set_qs_params']
                ], true);
          
          let attachCallbacks = (n) => {
                let el = $(n);
                
                if(!el.is('form') && el.find('button').length === 0) {
                
                  validateControl(el);
                
                  let input = el.find('select, input');
                  
                  let inputValue = getInputValue(el);
                  let defaultValue = getDefaultInputValue(el);
                  let name = el.data('rcloud-params-name');
                  
                  setUrlQuery(name, inputValue, defaultValue);
                  
                  input.on('change', function(e) {
                      validateControl(el);
                  });
                  
                  el.on('change', function (e) {
                      let val = getInputValue(el);
                      let input = el.find('select, input');
                      let defaultValue = getDefaultInputValue(el);
                      let name = el.data('rcloud-params-name');
      
                      if (val === '') val = undefined;
                      setUrlQuery(name, val, defaultValue);
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
                } else if(el.is('form')) {
                  if(!isSubmitEventBound(el)) {
                    el.submit(onFormSubmit(el, 
                        function(el, values) {
                            invokeBackend(el, el.get(0).name, values, {type : 'submit' } );
                        }));
                  }
                }
          };
          
          if (window.RCloudParams) {
            if (RCloudParams.observer) {
              RCloudParams.observer.disconnect();
            }
            if (RCloudParams.notebook_on_load_callback) {
              editor.remove_on_load_callback(RCloudParams.notebook_on_load_callback);
            }
          }

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
          
          window.RCloudParams = {
            observer
          };
          
          if (window.editor && window.shell && !shell.is_view_mode()) {
            // RCloud updates URL after initializing the session, this means that stale url is parsed when a user switches between notebooks
            // to handle this register a callback that will inject query parameters into R once the notebook got loaded
            RCloudParams.notebook_on_load_callback = function() {
              _varmap = url_utils.getQueryParams();
              _backend.set_qs_params(_varmap);
            }
            editor.add_on_load_callback(RCloudParams.notebook_on_load_callback);
          }
          
          _varmap = url_utils.getQueryParams();
          k(null, _varmap);
        },
        
        waitForReactiveForm:  function (context_id, form_id, k) {
          try {
             executeInCellResultProcessingLoop(context_id, function(result_div) {
                  let form = $('form[name="'+ form_id + '"]');
                  if (form.length > 0) {
    
                    validateForm(form);
                    form.off('submit');
                    form.submit(onFormSubmit(form, 
                      function(form, values) {
                          invokeBackend(form, form.get(0).name, values, {type : 'submit' } );
                      }));
                    
                    form.submit();
                  }
                  
              });
            } catch (err) {
              console.error(err);
            } finally {
              k();
              return true;
            }
        },
        
        waitForForm: function (context_id, form_id, k) {
            disableCallbacksForForm(form_id);
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let form = $('form[name="'+ form_id + '"]');
              if(form.length > 0) {
                
                validateForm(form);
                form.off('submit');
                
                form.submit(onFormSubmit(form, 
                  function(form, values) {
                    k(null, values);
                    enableCallbacksForForm(form.get(0).name);
                    form.find('button[type="submit"]').attr('disabled', 'disabled');
                  }, 
                  function(form, err) {
                    k(err, null);
                    enableCallbacksForForm(form.get(0).name);
                    form.find('button[type="submit"]').attr('disabled', 'disabled');
                  }));
              }
          });
        },
        
        validateForm: function (context_id, form_id, k) {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
                let form = $('form[name="'+ form_id + '"]');
                if(form.length > 0) {
                  
                  validateForm(form);
                  let invalidControls = form.find('.has-error');
                  if (invalidControls.length === 0) {
                    k(null, true);
                    return;
                  } else {
                    k(null, false);
                    return;
                  }
                }
                k(null, true);
          });
        },
        
        hideCellSource: function(cell_id, k) {
          try {
            let matching_cells = _.filter(shell.notebook.model.cells, (c) => { 
              return c.id() == cell_id; 
            });
            if (matching_cells.length > 0) {
              _.forEach(matching_cells, (c) => {
                _.forEach(c.views, (v) => {
                  if(v.hide_source) {
                    v.hide_source(true);
                  }
                })
              });
            } else {
              console.error("Cell with id " + cell_id + " not found!");
            }
          } finally {
            k();
          }
        },
        
        hideCurrentCellSource: function(context_id, k) {
          try {
            executeInCellResultProcessingLoop(context_id, function(result_div) {
              let filename = result_div.closest('.notebook-cell').get(0).id;
              let cell = _.filter(shell.notebook.view.model.cells, (c) => { return filename === c.filename(); });
              if(cell.length === 0) {
                console.error('Cell for context ${context_id} not found.');
              } else {
                _.forEach(cell, (c) => { _.forEach(c.views, (v) => { 
                  if(v.hide_source) v.hide_source(true); 
                })});
              }
            })
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
