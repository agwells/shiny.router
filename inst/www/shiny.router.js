/**
 * A good old-fashioned ES5 fake module.
 * See https://medium.com/@crohacz_86666/basics-of-modular-javascript-2395c82dd93a
 * @todo: Turn this into a good new-fashioned ES6 module?
 * @todo: minify.
 */
window.shiny_router = function() {
  const ROUTE_INPUT = '_shiny_router_path'

  current_context = false;

  /**
   * A reference to the callback function that Shiny sends us, which we're
   * meant to call whenever the input has updated.
   */
  var shinyTriggerUpdateFn = false;

  /**
   * To fit into the Shiny lifecycle nicely, we'll use an InputBinding. Normally
   * this is meant to track a specific DOM element, but we'll just return a phantom
   * empty jQuery object, and ignore whatever element gets passed in to the other
   * functions.
   */
  var inputBinding = new Shiny.InputBinding();
  jQuery.extend(inputBinding, {
    find: function(scope) {
      return jQuery({id: ROUTE_INPUT});
    },
    getId: function(el) {
      return ROUTE_INPUT
    },
    /**
     * When it tries to get the value of the input, we just retrieve it from
     * the cookie (if present)
     */
    getValue: function(el) {
      console.log("shiny.router::getValue", current_context);
      if (current_context) {
        return {
          path: current_context.pathname,
          path_and_query: current_context.path
        };
      } else {
        return false;
      }
    },
    setValue: function(el, value) {},
    subscribe: function(el, callback) {

      page({hashbang: true});
      page("*", function(context, next){
        console.log("Middleware called", context, next);
        // So, this eliminates the route validation. But it also eliminates the
        // need to communicate the routes back from the server-side to the
        // client-side!
        current_context = context;
        if (false !== shinyTriggerUpdateFn) {
          //shinyTriggerUpdateFn();
        }
      })

      // Make the callback available for us to invoke it manually.
      shinyTriggerUpdateFn = callback;
    },
    unsubscribe: function(el) {
      shinyTriggerUpdateFn = false;
    },
    receiveMessage: function(el, data) {
      console.log("shiny.router::receiveMessage", el, data);
      // We can use messages from Shiny to change the currently loaded page.
      page(data);
    }
  });

//  Shiny.inputBindings.register(inputBinding, "shiny.router");

  /**
   * The things that will be accessible under window.nzcerapi
   */
  return {
    // We don't actually need to return anything right now. The API is entirely
    // self-contained, and only does anything when initialized.
  };
}();