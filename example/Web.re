open Choop;
open Choop.Html;
open Choop.Html.Attributes;

type state = {
  mutable increment: int
};

let app = App.make();

App.use(app, (state, emitter) => {
  Js.log(state.increment);
  Emitter.emit(emitter, "stuff", 10);
});

App.route(app, "/", (_, _) => {
  main([_class("main-content")], 
    [
      div([_class("section__a")], [encodedText("aa")]),
      div([_class("section__b")], [])
    ]
  )
});

App.mount(app, "body");
