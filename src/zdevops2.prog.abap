*&---------------------------------------------------------------------*
*& Report ZDEVOPS2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdevops2.

TYPES: BEGIN OF ty_fields,
         systemareapath      TYPE string,
         systemiterationpath TYPE string,
       END OF ty_fields,

       BEGIN OF ty_data,
         id(6)  TYPE c,
         fields TYPE ty_fields,
       END OF ty_data.

DATA: lt_string TYPE TABLE OF string,
      lt_data   TYPE ty_data.

DATA: get_workitem   TYPE string,
      json_body      TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE,
      it_contents    TYPE STANDARD TABLE OF solisti1,
      it_response    TYPE STANDARD TABLE OF solisti1,
      json_response  TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE,
      lv_url         TYPE string,
      lv_response    TYPE string,
      v_atv          TYPE string,
      v_saida        TYPE string,
      lo_http_client TYPE REF TO if_http_client,
      lv_json_data   TYPE string.

DATA: lt_CreateWorkItem TYPE STANDARD TABLE OF string WITH HEADER LINE.

DATA : itab                TYPE string,
       teamproject_value   TYPE string,
       tags_value          TYPE string,
       title_value         TYPE string,
       iterationpath_value TYPE string,
       areapath_value      TYPE string,
       prazo_value         TYPE string,
       state_value         TYPE string,
       reason_value        TYPE string,
       assignedto_value    TYPE string,
       activity_value      TYPE string,
       workitemtype_value  TYPE string,
       ligacao_value       TYPE string,
       relations_value     TYPE string,
       type_value          TYPE string,
       v_url               TYPE string,
       authenticate_api    TYPE string.

CONSTANTS: c_ktk    TYPE string VALUE 'yqysbsa4d3y2mpbuckdiekq3wmvfrmd5ebrrf7rwrdr32jykv6cq',
           c_utk    TYPE string VALUE 'pablo alves',
           c_prazo  TYPE string VALUE '"op": "add","path": "/fields/Custom.PrazodaAtividade","from": null,"value": ',
           c_tproj  TYPE string VALUE '"op": "add","path": "/fields/System.TeamProject","from": null,"value": ',
           c_tags   TYPE string VALUE '"op": "add","path": "/fields/System.Tags","from": null,"value": ',
           c_title  TYPE string VALUE '"op": "add","path": "/fields/System.Title","from": null,"value": ',
           c_ipath  TYPE string VALUE '"op": "add","path": "/fields/System.IterationPath","from": null,"value": ',
           c_apath  TYPE string VALUE '"op": "add","path": "/fields/System.AreaPath","from": null,"value": ',
           c_state  TYPE string VALUE '"op": "add","path": "/fields/System.State","from": null,"value": ',
           c_reason TYPE string VALUE '"op": "add","path": "/fields/System.Reason","from": null,"value": ',
           c_assto  TYPE string VALUE '"op": "add","path": "/fields/System.AssignedTo","from": null,"value": ',
           c_activ  TYPE string VALUE '"op": "add","path": "/fields/Microsoft.VSTS.Common.Activity","from": null,"value": ',
           c_rwork  TYPE string VALUE '"op": "add","path": "Microsoft.VSTS.Scheduling.RemainingWork","from": null,"value": ',
           c_oesti  TYPE string VALUE '"op": "add","path": "Microsoft.VSTS.Scheduling.OriginalEstimate","from": null,"value": ',
           c_wtype  TYPE string VALUE '"op": "add","path": "/fields/System.WorkItemType","from": null,"value": ',
           c_lapiw  TYPE string VALUE 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workItems/'.





SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE t1.
  "PARAMETERS: p_us01 TYPE string  LOWER CASE OBLIGATORY.
  "p_dt01 TYPE sy-datum OBLIGATORY DEFAULT sy-datum.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_hist FOR FIELD p_hist.
    PARAMETERS:         p_hist(6) TYPE n LOWER CASE. "OBLIGATORY
    SELECTION-SCREEN PUSHBUTTON 22(8) but1 USER-COMMAND pb01.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN SKIP 1.

  PARAMETERS: p_area  TYPE string LOWER CASE,
              p_integ TYPE text1024 LOWER CASE.

SELECTION-SCREEN END OF BLOCK a.
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE t2.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv01 FOR FIELD p_atv01.
    PARAMETERS : p_atv01          TYPE boole_d DEFAULT ' '.    "Dev
    SELECTION-SCREEN COMMENT 18(3) t_tatv01 FOR FIELD p_tatv01.
    PARAMETERS :           p_tatv01(4)      TYPE c LOWER CASE.
    SELECTION-SCREEN COMMENT 30(2) t_dt01 FOR FIELD p_dt01.
    PARAMETERS : p_dt01 TYPE sy-datum." OBLIGATORY DEFAULT sy-datum.
    SELECTION-SCREEN COMMENT 45(4) t_us01 FOR FIELD p_us01.
    PARAMETERS : p_us01 TYPE char25."OBLIGATORY.
    SELECTION-SCREEN COMMENT 79(6) t_ttatv1 FOR FIELD p_ttatv1.
    PARAMETERS :           p_ttatv1      TYPE string LOWER CASE.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv02 FOR FIELD p_atv02.
    PARAMETERS : p_atv02     TYPE boole_d DEFAULT ' '.    "Test Dev
    SELECTION-SCREEN COMMENT 18(3) t_tatv02 FOR FIELD p_tatv02.
    PARAMETERS :             p_tatv02(4) TYPE c LOWER CASE.
    SELECTION-SCREEN COMMENT 30(2) t_dt02 FOR FIELD p_dt02.
    PARAMETERS : p_dt02 TYPE sy-datum." OBLIGATORY DEFAULT sy-datum. "OBLIGATORY
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv03 FOR FIELD p_atv03.
    PARAMETERS :            p_atv03     TYPE boole_d DEFAULT ' '.    "Documentação e Cutover
    SELECTION-SCREEN COMMENT 18(3) t_tatv03 FOR FIELD p_tatv03.
    PARAMETERS :            p_tatv03(4) TYPE c LOWER CASE.
    SELECTION-SCREEN COMMENT 30(2) t_dt03 FOR FIELD p_dt03.
    PARAMETERS : p_dt03 TYPE sy-datum." OBLIGATORY DEFAULT sy-datum."OBLIGATORY
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv04 FOR FIELD p_atv04.
    PARAMETERS :            p_atv04     TYPE boole_d DEFAULT ' '.    "Request
    SELECTION-SCREEN COMMENT 30(2) t_dt04 FOR FIELD p_dt04.
    PARAMETERS : p_dt04 TYPE sy-datum."OBLIGATORY DEFAULT sy-datum. "OBLIGATORY
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv05 FOR FIELD p_atv05.
    PARAMETERS :            p_atv05     TYPE boole_d DEFAULT ' '.    "QA
    SELECTION-SCREEN COMMENT 18(3) t_tatv05 FOR FIELD p_tatv05.
    PARAMETERS :            p_tatv05(4) TYPE c LOWER CASE.
    SELECTION-SCREEN COMMENT 30(2) t_dt05 FOR FIELD p_dt05.
    PARAMETERS : p_dt05 TYPE sy-datum ."OBLIGATORY DEFAULT sy-datum. "OBLIGATORY
    SELECTION-SCREEN COMMENT 45(4) t_us05 FOR FIELD p_us05.
    PARAMETERS : p_us05 TYPE char25." OBLIGATORY.
    SELECTION-SCREEN COMMENT 79(6) t_ttatv5 FOR FIELD p_ttatv5.
    PARAMETERS :           p_ttatv5      TYPE string LOWER CASE.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv06 FOR FIELD p_atv06.
    PARAMETERS :            p_atv06     TYPE boole_d DEFAULT ' '.    "Planejamento
    SELECTION-SCREEN COMMENT 18(3) t_tatv06 FOR FIELD p_tatv06.
    PARAMETERS :            p_tatv06(4) TYPE c LOWER CASE.
    SELECTION-SCREEN COMMENT 30(2) t_dt06 FOR FIELD p_dt06.
    PARAMETERS : p_dt06 TYPE sy-datum." OBLIGATORY DEFAULT sy-datum. "OBLIGATORY
    SELECTION-SCREEN COMMENT 45(4) t_us06 FOR FIELD p_us06.
    PARAMETERS : p_us06 TYPE char25." OBLIGATORY.
    SELECTION-SCREEN COMMENT 79(6) t_ttatv6 FOR FIELD p_ttatv6.
    PARAMETERS :           p_ttatv6      TYPE string LOWER CASE.
  SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN PUSHBUTTON 22(8) but2 USER-COMMAND pb02.

SELECTION-SCREEN END OF BLOCK b.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'P_AREA'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
    IF screen-name = 'P_INTEG'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  DATA(bt_buscar) = sy-ucomm.

  IF bt_buscar = 'PB01'.
    "PERFORM get_request.
  ELSEIF bt_buscar = 'PB02'.
    CLEAR: p_atv01,p_atv02,p_atv03,p_atv04,p_atv05,p_atv06.
    CLEAR: p_dt01,p_dt02,p_dt03,p_dt04,p_dt05,p_dt06.
    CLEAR: p_us01,p_us05,p_us06.
    CLEAR: p_tatv01,p_tatv02,p_tatv03,p_tatv05,p_tatv06.
    CLEAR: p_ttatv1,p_ttatv5,p_ttatv6.
  ENDIF.


INITIALIZATION.


START-OF-SELECTION.

  "Definições Constantes
  DATA(urlhist) = |{ c_lapiw }{ p_hist }|.
  DATA(c_rel) = '{' && |"rel": "System.LinkTypes.Hierarchy-Reverse","url": "{ urlhist }" | && '}'.
  DATA(relacao) = '{' && |"op": "add","path": "/relations/-","value": { c_rel }| && '}'.
  DATA(razao) = '{' && |{ c_reason }"Moved to state To Do"| && '}'.
  DATA(status)      = '{' && |{ c_state }"To Do"| && '}'.
  DATA(tipo)        = '{' && |{ c_wtype }"Task"| && '}'.

  IF  p_atv01 = 'X'. "Dev

    data(v_data) = |{ p_dt01+6(2) }-{ p_dt01+4(2) }-{ p_dt01+0(4) }|.
    data(v_title) = |{ v_data } - Dev { p_hist } - { p_ttatv1 }| .
    v_atv = 'Development'.
    data(v_tatv01) = |{ ( p_tatv01 ) * 1 }|.
    data(v_prazo) = |{ p_dt01+0(4) }-{ p_dt01+4(2) }-{ p_dt01+6(2) }T22:00:00|.

    CLEAR: lt_CreateWorkItem.
    DATA(prazo)       = '{' && |{ c_prazo }"{ v_prazo }"| && '}'.
    "DATA(tags)        = '{' && |{ c_tags }"{ x }"| && '}'.
    DATA(projeto)     = '{' && |{ c_tproj }"{ p_area }"| && '}'.
    DATA(titulo)      = '{' && |{ c_title }"{ v_title }"| && '}'.
    DATA(interacao)   = '{' && |{ c_ipath }"{ p_integ }"| && '}'.
    DATA(area)        = '{' && |{ c_apath }"{ p_area }"| && '}'.
    DATA(usuario)     = '{' && |{ c_assto }"{ p_us01 }"| && '}'.
    DATA(atividade)   = '{' && |{ c_activ }"{ v_atv }"| && '}'.
    DATA(remanecente) = '{' && |{ c_rwork }"{ v_tatv01 }"| && '}'.
    DATA(estimativa)  = '{' && |{ c_oesti }"{ v_tatv01 }"| && '}'.

    lt_CreateWorkItem = |[{ prazo },{ projeto },{ titulo },{ interacao },{ area },{ usuario },{ status },{ razao },{ atividade },{ remanecente },{ estimativa },{ tipo },{ relacao }];|.

*    PERFORM send_request.
  ENDIF.
  IF  p_atv02 = 'X'. "Test Dev
*    PERFORM limpa_memoria.
*    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.

*    v_atv = 'Atividade Desenvolvimento Teste'.
*    v_tatv02 = |{ ( p_tatv02 ) * 1 }|.
*    v_prazo = |{ p_dt02+0(4) }-{ p_dt02+4(2) }-{ p_dt02+6(2) }T22:00:00|.

    data(v_data) = |{ p_dt01+6(2) }-{ p_dt01+4(2) }-{ p_dt01+0(4) }|.
    data(v_title) = |{ v_data } - Test Dev { p_hist } - { p_ttatv1 }| .
    v_atv = 'Development'.
    data(v_tatv01) = |{ ( p_tatv01 ) * 1 }|.
    data(v_prazo) = |{ p_dt01+0(4) }-{ p_dt01+4(2) }-{ p_dt01+6(2) }T22:00:00|.

    CLEAR: lt_CreateWorkItem.
    DATA(prazo)       = '{' && |{ c_prazo }"{ v_prazo }"| && '}'.
    "DATA(tags)        = '{' && |{ c_tags }"{ x }"| && '}'.
    DATA(projeto)     = '{' && |{ c_tproj }"{ p_area }"| && '}'.
    DATA(titulo)      = '{' && |{ c_title }"{ v_title }"| && '}'.
    DATA(interacao)   = '{' && |{ c_ipath }"{ p_integ }"| && '}'.
    DATA(area)        = '{' && |{ c_apath }"{ p_area }"| && '}'.
    DATA(usuario)     = '{' && |{ c_assto }"{ p_us01 }"| && '}'.
    DATA(atividade)   = '{' && |{ c_activ }"{ v_atv }"| && '}'.
    DATA(remanecente) = '{' && |{ c_rwork }"{ v_tatv01 }"| && '}'.
    DATA(estimativa)  = '{' && |{ c_oesti }"{ v_tatv01 }"| && '}'.

    lt_CreateWorkItem = |[{ prazo },{ projeto },{ titulo },{ interacao },{ area },{ usuario },{ status },{ razao },{ atividade },{ remanecente },{ estimativa },{ tipo },{ relacao }];|.





*    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Tags","value": "Teste Dev" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.TeamProject","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv02' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv02' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
*    PERFORM send_request.
  ENDIF.
  IF p_atv03 = 'X'. "Documentação e Cutover
*    PERFORM limpa_memoria.
*    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
*    v_data = |{ p_dt03+6(2) }-{ p_dt03+4(2) }-{ p_dt03+0(4) }|.
*    v_title = |{ p_dt03+0(4) }-{ p_dt03+4(2) }-{ p_dt03+6(2) } - Documentação e Cutover { p_hist }| .
*    v_atv = 'Atividade Documentação e Cutover'.
*    v_tatv03 = |{ ( p_tatv03 ) * 1 }|.
*    v_prazo = |{ p_dt03+0(4) }-{ p_dt03+4(2) }-{ p_dt03+6(2) }T22:00:00|.
*    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.TeamProject","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Documentation" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv03' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv03' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
*    PERFORM send_request.
  ENDIF.
  IF  p_atv04 = 'X'. "Request
*    PERFORM limpa_memoria.
*    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$request?api-version=7.2-preview.3'.
*    v_data = |{ p_dt04+6(2) }-{ p_dt04+4(2) }-{ p_dt04+0(4) }|.
*    v_title = |{ v_data }  - Request { p_hist }| .
*    v_atv = 'Atividade Request'.
*    v_prazo = |{ p_dt04+0(4) }-{ p_dt04+4(2) }-{ p_dt04+6(2) }T22:00:00|.
*    CONCATENATE  '{'add_start'System.TeamProject","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.State","value": "New" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state New" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.WorkItemType","value": "Request" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
*    PERFORM send_request.
  ENDIF.
  IF p_atv05 = 'X'. "QA
*    PERFORM limpa_memoria.
*    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
*    v_data = |{ p_dt05+6(2) }-{ p_dt05+4(2) }-{ p_dt05+0(4) } |.
*    v_title = |{ p_dt05+0(4) }-{ p_dt05+4(2) }-{ p_dt05+6(2) }  - QA { p_hist } - {  p_ttatv5 }| .
*    v_atv = 'Atividade QA'.
*    v_tatv05 = |{ ( p_tatv05 ) * 1 }|.
*    v_prazo = |{ p_dt05+0(4) }-{ p_dt05+4(2) }-{ p_dt05+6(2) }T22:00:00|.
*    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.TeamProject","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us05'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Testing" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv05' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv05' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.WorkItemType","value": "Testing" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
*    PERFORM send_request.
  ENDIF.
  IF p_atv06 = 'X'. "Planejamento
*    PERFORM limpa_memoria.
*    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
*    v_data = |{ p_dt06+6(2) }-{ p_dt06+4(2) }-{ p_dt06+0(4) } |.
*    v_title = |{ v_data } - Planejamento - {  p_ttatv6 }| .
*    v_atv = 'Atividade Planejamento'.
*    v_tatv06 = |{ ( p_tatv06 ) * 1 }|.
*    v_prazo = |{ p_dt06+0(4) }-{ p_dt06+4(2) }-{ p_dt06+6(2) }T22:00:00|.
*    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP" }' INTO  json_body. APPEND json_body. "p_area
*    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.IterationPath","value": "SAP" }' INTO  json_body. APPEND json_body. "p_integ
*    CONCATENATE  '{'add_start'System.AreaPath","value": "SAP" }' INTO  json_body. APPEND json_body. "'p_area'
*    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us06'" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv06' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv06' }' INTO  json_body. APPEND json_body.
*    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
*    PERFORM send_request.
  ENDIF.

*  PERFORM limpa_memoria.

  SORT it_response[].
  DELETE ADJACENT DUPLICATES FROM it_response[].

  cl_demo_output=>display( it_response ).
  FREE: it_response[].


*  PERFORM atv.
*
*
*  cl_demo_output=>display( lt_CreateWorkItem ).



END-OF-SELECTION.

*FORM atv.
*  CLEAR: lt_CreateWorkItem.
*  DATA(x)           = 1.
*  DATA(prazo)       = '{' && |{ c_prazo }"{ x }"| && '}'.
*  DATA(tags)        = '{' && |{ c_tags }"{ x }"| && '}'.
*  DATA(projeto)     = '{' && |{ c_tproj }"{ x }"| && '}'.
*  DATA(titulo)      = '{' && |{ c_title }"{ x }"| && '}'.
*  DATA(interacao)   = '{' && |{ c_ipath }"{ x }"| && '}'.
*  DATA(area)        = '{' && |{ c_apath }"{ x }"| && '}'.
*  DATA(usuario)     = '{' && |{ c_assto }"{ x }"| && '}'.
*  DATA(status)      = '{' && |{ c_state }"{ x }"| && '}'.
*  DATA(atividade)   = '{' && |{ c_activ }"{ x }"| && '}'.
*  DATA(remanecente) = '{' && |{ c_rwork }"{ x }"| && '}'.
*  DATA(estimativa)  = '{' && |{ c_oesti }"{ x }"| && '}'.
*  DATA(tipo)        = '{' && |{ c_wtype }"{ x }"| && '}'.
*
*  lt_CreateWorkItem = |[{ tags }{ prazo },{ projeto },{ titulo },{ interacao },{ area },{ usuario },{ status },{ razao },{ atividade },{ remanecente },{ estimativa },{ tipo },{ relacao }];|.
*
*ENDFORM.

FORM gera_autorization.

  DATA: lv_xstring TYPE xstring,       "Xstring
        lv_len     TYPE i,             "Length
        lt_content TYPE soli_tab,      "Content
        lv_string  TYPE string,        "Text
        lv_base64  TYPE string.        "Base64

  lv_string = |{ c_utk }:{ c_ktk }|. "Replace with your actual username and password

*Convert string to xstring

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_string
    IMPORTING
      buffer = lv_xstring
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

*Find the number of bites of xstring
  lv_len  = xstrlen( lv_xstring ).

  CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
    EXPORTING
      input  = lv_xstring
    IMPORTING
      output = lv_base64.

  authenticate_api  = |Basic { lv_base64 }|.
ENDFORM.

FORM get_request.

  DATA: lo_get_response TYPE REF TO if_http_client,
        lv_service      TYPE string.

  DATA(get_workitem) = |{ urlhist }?api-version=7.1-preview.3|.

  cl_http_client=>create_by_url(
      EXPORTING
        url                = get_workitem
      IMPORTING
        client             = lo_get_response
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
  ).


  " TRY.
  lo_get_response->request->set_header_field( name  = 'Content-Type' value = 'application/json; charset=utf-8' ).
  lo_get_response->request->set_header_field( name  = 'Authorization' value = authenticate_api ).
  lo_get_response->request->set_method( 'GET' ).
  "lo_get_response->request->set_cdata( ).
  lo_get_response->send(       EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4
    OTHERS                     = 5 ).

  CHECK sy-subrc = 0.

  lo_get_response->receive(
  EXCEPTIONS
http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    OTHERS                     = 4 ).
  CLEAR:lv_service,lt_data.
  lv_service =  lo_get_response->response->get_cdata( ).
  REPLACE ALL OCCURRENCES OF '.' IN lv_service WITH ||.

  /ui2/cl_json=>deserialize( EXPORTING json = lv_service
    pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                              CHANGING data = lt_data ).

  CHECK sy-subrc = 0.

  CLEAR: p_area.
  p_area = lt_data-fields-systemareapath.
  REPLACE ALL OCCURRENCES OF '\' IN p_area WITH '\\'.
  p_integ = lt_data-fields-systemiterationpath.
  REPLACE ALL OCCURRENCES OF '\' IN p_integ WITH '\\'.
ENDFORM.

FORM send_request.

  APPEND LINES OF json_body TO it_contents.
  DATA(virgula) = ','.
  CONCATENATE LINES OF it_contents INTO DATA(json_data)  SEPARATED BY virgula.

  lv_json_data = '[' && json_data && ']'.

  CLEAR: json_data,json_data,json_body,it_contents.
  FREE: it_contents,json_body.

  cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
  ).


  TRY.
      lo_http_client->request->set_header_field( name  = 'Content-Type' value = 'application/json-patch+json' ).
      lo_http_client->request->set_header_field( name  = 'Authorization' value = authenticate_api ).
      lo_http_client->request->set_method( 'POST' ).
      lo_http_client->request->set_cdata( lv_json_data ).
      lo_http_client->send( ).
      lo_http_client->receive(
      EXCEPTIONS
      http_communication_failure = 1
http_invalid_state         = 2
http_processing_failed     = 3 ).

      IF sy-subrc = 0.
        lv_response = lo_http_client->response->get_cdata( ).
        "WRITE: / 'HTTP Response:', lv_response.
        v_saida = |{ v_atv }: { lv_response }|.
        json_response = v_saida.
        APPEND json_response TO it_response.
      ELSE.
        "WRITE: / 'HTTP Request Failed:', sy-subrc.
        v_saida = |{ v_atv }: { sy-subrc }|.
        json_response = v_saida.
        APPEND json_response TO it_response.
      ENDIF.
    CATCH cx_root INTO DATA(e).
      "WRITE: / 'Exception:', e->get_text( ).
      v_saida = |{ v_atv }: { e->get_text( ) }|.
      json_response = v_saida.
      APPEND json_response TO it_response.
  ENDTRY.

  "PERFORM limpa_memoria.

ENDFORM.
"DEV
"DEV TESTES
"Tags
"DOCUMENTAÇÃO E CUTOVER
"REQUEST
"QA
"Planejamento
