module ComposableMap = {
  [@bs.val] [@bs.module "react-simple-maps"]
  external composableMap : ReasonReact.reactClass = "ComposableMap";
  let make = children =>
    ReasonReact.wrapJsForReason(
      ~reactClass=composableMap,
      children,
      ~props=(),
    );
};

module ZoomableGroup = {
  [@bs.val] [@bs.module "react-simple-maps"]
  external zoomableGroup : ReasonReact.reactClass = "ZoomableGroup";
  let make = children =>
    ReasonReact.wrapJsForReason(
      ~reactClass=zoomableGroup,
      children,
      ~props=(),
    );
};
module Markers = {
  [@bs.val] [@bs.module "react-simple-maps"]
  external markers : ReasonReact.reactClass = "Markers";
  let make = children =>
    ReasonReact.wrapJsForReason(~reactClass=markers, children, ~props=());
};

module Geographies = {
  [@bs.deriving abstract]
  type jsProps = {geography: string};

  [@bs.val] [@bs.module "react-simple-maps"]
  external geographies : ReasonReact.reactClass = "Geographies";
  let make = (~onData, _) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=geographies,
      (geographies, projection) => onData(geographies, projection),
      ~props=jsProps(~geography="world-50m.json"),
    );
};

module Marker = {
  [@bs.deriving abstract]
  type markerData = {coordinates: array(float)};
  [@bs.deriving abstract]
  type jsProps = {marker: markerData};

  [@bs.val] [@bs.module "react-simple-maps"]
  external marker : ReasonReact.reactClass = "Marker";
  let make = (~coordinates, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=marker,
      ~props=jsProps(~marker=markerData(~coordinates)),
      children,
    );
};

module Geography = {
  type geographyData = {. "id": string};
  [@bs.deriving abstract]
  type jsProps = {
    geography: geographyData,
    projection: string,
  };

  [@bs.val] [@bs.module "react-simple-maps"]
  external geography : ReasonReact.reactClass = "Geography";
  let make = (~data, ~projection, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=geography,
      children,
      ~props=jsProps(~geography=data, ~projection),
    );
};
type state =
  | Data(array(Fetcher.data))
  | Loading
  | NoData;

type action =
  | LoadData
  | DataLoaded(array(Fetcher.data));

let component = ReasonReact.reducerComponent("Map");

let make = _children => {
  ...component,
  initialState: () => NoData,
  reducer: (action, _) =>
    switch (action) {
    | DataLoaded(returnedData) =>
      Js.log("DATA LOADED");
      ReasonReact.Update(Data(returnedData));
    | LoadData =>
      ReasonReact.UpdateWithSideEffects(
        Loading,
        (
          self =>
            Fetcher.fetchGet(
              ~url="https://immense-river-25513.herokuapp.com/locations",
              ~cb=data =>
              self.send(DataLoaded(data))
            )
        ),
      )
    },
  didMount: self => self.send(LoadData),
  render: _self =>
    <ComposableMap>
      <ZoomableGroup>
        <Geographies
          onData=(
            (geographies, projection) =>
              Belt.Array.map(geographies, geography =>
                <Geography key=geography##id data=geography projection />
              )
          )
        />
        (
          switch (_self.state) {
          | NoData => ReasonReact.null
          | Loading => ReasonReact.null
          | Data(data) =>
            <Markers>
              (
                data
                |. Belt.Array.map(data => {
                     let (x, y) = Fetcher.location(data);
                     ([|y, x|], Fetcher.username(data));
                   })
                |. Belt.Array.map(((coordinates,username)) =>
                     <Marker coordinates>
                       <circle fill="red" cx="0" cy="0" r="3" />
                       <text fill="blue" x="0" y="0">(ReasonReact.string(username))</text>
                     </Marker>
                   )
              )
            </Markers>
          }
        )
      </ZoomableGroup>
    </ComposableMap>,
};