open Choop;
open Choop.Html;
open Choop.Html.Attributes;

type state = {
  mutable count: int
};

let app = App.make();

App.use(app, (state, emitter) => {
  state.count = 0;
  Emitter.on(emitter, "increment", count => {
    state.count = state.count + count;
    Emitter.emit(emitter, "render", ());
  });
});

App.route(app, "/", ({count}, emit) => {
  let onclick = () => emit(. "increment", 1);
  
  main([_class("main-content")],
    [
      h1(
         [_class("main-content__header")],
         [text({j| Count is $count |j})]
      ),
      button([_onclick(onclick)], [text("Click me")]),
      div([_dangerouslySetInnerHTML("<div>I am dangerous!</div>")], [])
    ]
  )
});

App.mount(app, "body");
