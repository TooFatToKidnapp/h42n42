
open%shared Eliom_content.Html.F

let%shared elm = div ~a:[a_class ["about"]] [
  h1 ~a:[] [txt "H42N42 - Creet Colony"];

  p ~a:[] [txt "Manage a colony of creatures called 'Creets'. Keep them healthy and prevent contamination."];

  h2 ~a:[] [txt "Controls"];
  ul ~a:[] [
    li ~a:[] [txt "Drag creets with your mouse"];
    li ~a:[] [txt "Drag sick creets to hospital (bottom) to heal them"]
  ];

  h2 ~a:[] [txt "Creet States"];
  ul ~a:[] [
    li ~a:[] [txt "🔵 Healthy - Normal behavior"];
    li ~a:[] [txt "🟤 Sick - Slower, can infect others"];
    li ~a:[] [txt "🔴 Berserk - Grows larger, aggressive"];
    li ~a:[] [txt "🟣 Mean - Hunts healthy creets, shrinks"]
  ];

  h2 ~a:[] [txt "Zones"];
  ul ~a:[] [
    li ~a:[] [txt "🌊 River (top) - Contaminates healthy creets"];
    li ~a:[] [txt "🏥 Hospital (bottom) - Heals sick creets"]
  ];

  p ~a:[] [txt "Goal: Keep your colony alive by managing contamination and healing sick creets."]
]
