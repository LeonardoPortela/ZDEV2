*&---------------------------------------------------------------------*
*& Report ZDEVOPS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdevops.

DATA: lv_xstring TYPE xstring,       "Xstring
      lv_len     TYPE i,             "Length
      lt_content TYPE soli_tab,      "Content
      lv_string  TYPE string,        "Text
      lv_base64  TYPE string.        "Base64

DATA: lo_http_client   TYPE REF TO if_http_client,
      request          TYPE REF TO if_http_request,
      response         TYPE REF TO if_http_response,
      "      url              TYPE text1024,
      content          TYPE text1024,
      p_url            TYPE text1024,
      result           TYPE i,
      authenticate_api TYPE string.

DATA: type_request    TYPE string,
      type_task       TYPE string,
      endpoint        TYPE string,
      lv_url          TYPE string,
      lv_username     TYPE string,
      lv_password     TYPE string,
      lv_auth_header  TYPE string,
      lv_response     TYPE string,
      lv_response_get TYPE string,
      lv_json_data    TYPE string,
      lv_json_get     TYPE string,
      v_url           TYPE text1024,
      v_usertk        TYPE text1024,
      v_token         TYPE text1024,
      v_title         TYPE text1024,
      v_atv           TYPE string,
      v_saida         TYPE string,
      v_tag           TYPE text1024,
      to_do           TYPE text1024,
      move_to         TYPE text1024,
      atv_type        TYPE text1024,
      v_task          TYPE text1024,
      v_rel           TYPE text1024,
      v_user          TYPE text1024,
      add_start       TYPE text1024,
      virgula(1)      TYPE c,
      atividade       TYPE text1024,
      v_tatv01        TYPE string,
      v_tatv02        TYPE string,
      v_tatv03        TYPE string,
      v_tatv05        TYPE string,
      v_tatv06        TYPE string,
      b_dupla         TYPE string,
      linkazure       TYPE text1024,
      v_assoc         TYPE string,
      v_data          TYPE string,
      v_prazo         TYPE string,
      area_path       TYPE text1024,
      get_workitem    TYPE string,
      json_body       TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE,
      it_contents     TYPE STANDARD TABLE OF solisti1,
      it_response     TYPE STANDARD TABLE OF solisti1,
      json_response   TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE.

TYPES: BEGIN OF ty_fields,
         systemareapath      TYPE string,
         systemiterationpath TYPE string,
         SystemTags          TYPE string,
       END OF ty_fields,

       BEGIN OF ty_data,
         id(6)  TYPE c,
         fields TYPE ty_fields,
       END OF ty_data.

DATA: lt_string TYPE TABLE OF string,
      lt_data   TYPE ty_data.

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
              p_integ TYPE text1024 LOWER CASE,
              p_stags TYPE text1024 LOWER CASE.

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
    PARAMETERS : p_us01 TYPE char29 MATCHCODE OBJECT ZUSUARIOS_ZDEVOPS."OBLIGATORY.
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
    PARAMETERS : p_us05 TYPE char29 MATCHCODE OBJECT ZUSUARIOS_ZDEVOPS." OBLIGATORY.
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
    PARAMETERS : p_us06 TYPE char29 MATCHCODE OBJECT ZUSUARIOS_ZDEVOPS." OBLIGATORY.
    SELECTION-SCREEN COMMENT 79(6) t_ttatv6 FOR FIELD p_ttatv6.
    PARAMETERS :           p_ttatv6      TYPE string LOWER CASE.
  SELECTION-SCREEN: END OF LINE.
*** Code Review - MMSILVA - 20.06.2025 ***
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) t_atv07 FOR FIELD p_atv07.
    PARAMETERS :            p_atv07     TYPE boole_d DEFAULT ' '.    "Code Review
    SELECTION-SCREEN COMMENT 30(2) t_dt07 FOR FIELD p_dt07.
    PARAMETERS : p_dt07 TYPE sy-datum." OBLIGATORY DEFAULT sy-datum. "OBLIGATORY
    SELECTION-SCREEN COMMENT 45(4) t_us07 FOR FIELD p_us07.
    PARAMETERS : p_us07 TYPE char29 MATCHCODE OBJECT ZUSUARIOS_ZDEVOPS." OBLIGATORY.
  SELECTION-SCREEN: END OF LINE.
*** Code Review - MMSILVA - 20.06.2025 ***

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
    IF screen-name = 'P_STAGS'.
      screen-input = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  DATA(bt_buscar) = sy-ucomm.

  IF bt_buscar = 'PB01'.
    PERFORM get_request.
  ELSEIF bt_buscar = 'PB02'.
    CLEAR: p_atv01,p_atv02,p_atv03,p_atv04,p_atv05,p_atv06.
    CLEAR: p_dt01,p_dt02,p_dt03,p_dt04,p_dt05,p_dt06.
    CLEAR: p_us01,p_us05,p_us06.
    CLEAR: p_tatv01,p_tatv02,p_tatv03,p_tatv05,p_tatv06.
    CLEAR: p_ttatv1,p_ttatv5,p_ttatv6.
  ENDIF.


INITIALIZATION.

  v_token = '7DaqcO4CdfKEwkhONW0ehH4W7pi1CoCnwsNOfm9h1QaWiIb7LaivJQQJ99BGACAAAAAJ4NZ8AAASAZDOiT4T'.
  v_usertk = 'suporte.sap@amaggi.com.br'.

  PERFORM gera_autorization.

  t1 = 'Parâmetros'.
  t2 = 'Atividades'.
  t_atv01 = 'Dev'.
  t_tatv01 = 'Hrs'.
  t_ttatv1 = 'Título'.
  t_atv02 = 'Test Dev'.
  t_tatv02 = 'Hrs'.
  t_atv03 = 'Doc Cutover'.
  t_tatv03 = 'Hrs'.
  t_atv04 = 'Request'.
  t_atv05 = 'QA'.
  t_tatv05 = 'Hrs'.
  t_ttatv5 = 'Título'.
  t_atv06 = 'Planejamento'.
  t_tatv06 = 'Hrs'.
  t_ttatv6 = 'Título'.
  t_hist = 'História ID:'.
  but1 = 'Buscar'.
  but2 = 'Limpar'.
  t_dt01 = 'Dt'.
  t_dt02 = 'Dt'.
  t_dt03 = 'Dt'.
  t_dt04 = 'Dt'.
  t_dt05 = 'Dt'.
  t_dt06 = 'Dt'.
  t_us01 = 'User'.
  t_us05 = 'User'.
  t_us06 = 'User'.
*** Code Review - MMSILVA - 20.06.2025 ***
  t_atv07   = 'Code Review'.
  t_dt07    = 'Dt'.
  t_us07    = 'User'.
*** Code Review - MMSILVA - 20.06.2025 ***

  PERFORM get_list_users.


START-OF-SELECTION.


  PERFORM constantes.

  REPLACE ALL OCCURRENCES OF '\' IN p_area WITH '\\'.
  REPLACE ALL OCCURRENCES OF '\' IN p_integ WITH '\\'.


  IF  p_atv01 = 'X'. "Dev
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt01+6(2) }-{ p_dt01+4(2) }-{ p_dt01+0(4) }|.
    v_title = |{ v_data } - Dev { p_hist } - { p_ttatv1 }| .
    v_atv = 'Atividade Desenvolvimento'.
    v_tatv01 = |{ ( p_tatv01 ) * 1 }|.
    v_prazo = |{ p_dt01+0(4) }-{ p_dt01+4(2) }-{ p_dt01+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv01' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv01' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
  IF  p_atv02 = 'X'. "Test Dev
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt02+6(2) }-{ p_dt02+4(2) }-{ p_dt02+0(4) }|.
    v_title = |{ v_data } - Test Dev { p_hist }| .
    v_atv = 'Atividade Desenvolvimento Teste'.
    v_tatv02 = |{ ( p_tatv02 ) * 1 }|.
    v_prazo = |{ p_dt02+0(4) }-{ p_dt02+4(2) }-{ p_dt02+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Tags","value": "Teste Dev" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv02' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv02' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
  IF p_atv03 = 'X'. "Documentação e Cutover
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt03+6(2) }-{ p_dt03+4(2) }-{ p_dt03+0(4) }|.
    v_title = |{ p_dt03+6(2) }-{ p_dt03+4(2) }-{ p_dt03+0(4) } - Documentação e Cutover { p_hist }| .
    v_atv = 'Atividade Documentação e Cutover'.
    v_tatv03 = |{ ( p_tatv03 ) * 1 }|.
    v_prazo = |{ p_dt03+0(4) }-{ p_dt03+4(2) }-{ p_dt03+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Documentation" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv03' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv03' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
  IF  p_atv04 = 'X'. "Request
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$request?api-version=7.2-preview.3'.
    v_data = |{ p_dt04+6(2) }-{ p_dt04+4(2) }-{ p_dt04+0(4) }|.
    v_title = |{ v_data }  - Request { p_hist }| .
    v_atv = 'Atividade Request'.
    v_prazo = |{ p_dt04+0(4) }-{ p_dt04+4(2) }-{ p_dt04+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us01'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "New" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state New" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Request" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
  IF p_atv05 = 'X'. "QA
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt05+6(2) }-{ p_dt05+4(2) }-{ p_dt05+0(4) } |.
    v_title = |{ p_dt05+6(2) }-{ p_dt05+4(2) }-{ p_dt05+0(4) }  - QA { p_hist } - {  p_ttatv5 }| .
    v_atv = 'Atividade QA'.
    v_tatv05 = |{ ( p_tatv05 ) * 1 }|.
    v_prazo = |{ p_dt05+0(4) }-{ p_dt05+4(2) }-{ p_dt05+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us05'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Testing" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv05' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv05' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Testing" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
  IF p_atv06 = 'X'. "Planejamento
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt06+6(2) }-{ p_dt06+4(2) }-{ p_dt06+0(4) } |.
    v_title = |{ v_data } - Planejamento - {  p_ttatv6 }| .
    v_atv = 'Atividade Planejamento'.
    v_tatv06 = |{ ( p_tatv06 ) * 1 }|.
    v_prazo = |{ p_dt06+0(4) }-{ p_dt06+4(2) }-{ p_dt06+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'Custom.PrazodaAtividade","value": "'v_prazo'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP" }' INTO  json_body. APPEND json_body. "p_area
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "SAP" }' INTO  json_body. APPEND json_body. "p_integ
    CONCATENATE  '{'add_start'System.AreaPath","value": "SAP" }' INTO  json_body. APPEND json_body. "'p_area'
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us06'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.RemainingWork","value": 'v_tatv06' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Scheduling.OriginalEstimate","value": 'v_tatv06' }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Task" }' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
*** Code Review - MMSILVA - 20.06.2025 ***
  IF p_atv07 = 'X'. "Code Review
    PERFORM limpa_memoria.
    lv_url = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workitems/$task?api-version=7.2-preview.3'.
    v_data = |{ p_dt07+6(2) }-{ p_dt07+4(2) }-{ p_dt07+0(4) } |.
    v_title = |{ p_dt07+6(2) }-{ p_dt07+4(2) }-{ p_dt07+0(4) }  - Code Review { p_hist }| .
    v_atv = 'Atividade Code Review'.
    v_prazo = |{ p_dt07+0(4) }-{ p_dt07+4(2) }-{ p_dt07+6(2) }T22:00:00|.
    CONCATENATE  '{'add_start'System.TeamProject","value": "SAP"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Title","value": "'v_title'"}' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.IterationPath","value": "'p_integ'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AreaPath","value": "'p_area'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.AssignedTo","value": "'p_us07'" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.State","value": "To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.Reason","value": "Moved to state To Do" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'Microsoft.VSTS.Common.Activity","value": "Development" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{'add_start'System.WorkItemType","value": "Development" }' INTO  json_body. APPEND json_body.
    CONCATENATE  '{"op": "add","path": "/relations/-","value": {"rel":"System.LinkTypes.Hierarchy-Reverse","url":"'v_url'"}}' INTO  json_body. APPEND json_body.
    PERFORM send_request.
  ENDIF.
*** Code Review - MMSILVA - 20.06.2025 ***

  PERFORM limpa_memoria.

  SORT it_response[].
  DELETE ADJACENT DUPLICATES FROM it_response[].

  cl_demo_output=>display( it_response ).
  FREE: it_response[].

FORM gera_autorization.
  lv_string = |{ v_usertk }:{ v_token }|. "Replace with your actual username and password

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

FORM constantes.
  add_start = '"op": "add","path": "/fields/'.
  linkazure = 'https://dev.azure.com/Amaggi/SAP/_apis/wit/workItems/'.
  v_url = |{ linkazure }{ p_hist }|.
  v_rel = 'System.LinkTypes.Hierarchy-Reverse' .
  v_assoc = '{"rel":"' && v_rel && '","url":"' && v_url && '"}'.
  DATA(p_dt) = sy-datum.
  b_dupla = '\\'.

ENDFORM.

FORM send_request.

  APPEND LINES OF json_body TO it_contents.
  virgula = ','.
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

  PERFORM limpa_memoria.

ENDFORM.


FORM limpa_memoria.

  CLEAR: lv_url,lo_http_client,it_contents,json_body,lv_url,v_title,v_atv,v_saida,v_atv,lv_response,sy-subrc,lv_json_data,v_tatv01,v_tatv02,v_tatv03,v_tatv05,v_tatv06,v_prazo,v_data.
  FREE: it_contents,json_body.

ENDFORM.


FORM get_request.

  DATA: lo_get_response TYPE REF TO if_http_client,
        lv_service      TYPE string.

  get_workitem = |https://amaggi.visualstudio.com/SAP/_apis/wit/workitems/{ p_hist }?api-version=7.1-preview.3|.

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
  lo_get_response->send( EXCEPTIONS http_communication_failure = 1
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

  /ui2/cl_json=>deserialize( EXPORTING json        = lv_service
                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                             CHANGING  data        = lt_data ).

  CHECK sy-subrc = 0.

  CLEAR: p_area.
  p_area = lt_data-fields-systemareapath.
  REPLACE ALL OCCURRENCES OF '\' IN p_area WITH '\\'.
  p_integ = lt_data-fields-systemiterationpath.
  REPLACE ALL OCCURRENCES OF '\' IN p_integ WITH '\\'.
  p_stags = lt_data-fields-systemtags.
ENDFORM.

FORM get_list_users .
*  DATA: lo_get_response TYPE REF TO if_http_client,
*        lv_service      TYPE string.
*
*  TYPES: BEGIN OF ty_teste,
*           value TYPE string,
*         END OF ty_teste.
*
*  DATA: it_saida TYPE STANDARD TABLE OF ty_teste.
*
*
*  TYPES: BEGIN OF ty_fields,
*           displayName TYPE string,
*           url         TYPE string,
*           _links      TYPE string,
*           id          TYPE string,
*           uniqueName  TYPE string,
*           imageUrl    TYPE string,
*           descriptor  TYPE string,
*         END OF ty_fields.
*
*  TYPES: BEGIN OF ty_identity,
*           identity TYPE ty_fields,
*         END OF ty_identity.
*
*  TYPES: BEGIN OF ty_value,
*           value TYPE STANDARD TABLE OF ty_identity WITH EMPTY KEY,
*           count TYPE i,
*         END OF ty_value.
*
*  DATA: wa_value TYPE ty_value.
*  DATA: it_identity TYPE STANDARD TABLE OF ty_identity INITIAL SIZE 0,
*        wa_identity TYPE ty_identity.
*
*  TYPES: BEGIN OF ty_user,
*           name TYPE name,
*         END OF ty_user.
*  DATA: it_list_user TYPE STANDARD TABLE OF ty_user INITIAL SIZE 0,
*        wa_list_user TYPE ty_user.
*
*  DATA: lt_value TYPE ty_value.
*  DATA: it_value TYPE ty_value.

*  DATA: list  TYPE vrm_values,
*        value LIKE LINE OF list.
*
*  DATA(get_users_evo) = |https://amaggi.visualstudio.com/_apis/projects/SAP/teams/fbb8ba29-7b0d-4c34-8805-5b665855d646/members?api-version=7.1|. "Os Ninjas - Evolution"
*
*  cl_http_client=>create_by_url(
*    EXPORTING
*      url                = get_users_evo
*    IMPORTING
*      client             = lo_get_response
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      OTHERS             = 4
*  ).
*
*
*  " TRY.
*  lo_get_response->request->set_header_field( name  = 'Content-Type' value = 'application/json; charset=utf-8' ).
*  lo_get_response->request->set_header_field( name  = 'Authorization' value = authenticate_api ).
*  lo_get_response->request->set_method( 'GET' ).
*  "lo_get_response->request->set_cdata( ).
*  lo_get_response->send( EXCEPTIONS http_communication_failure = 1
*                                    http_invalid_state         = 2
*                                    http_processing_failed     = 3
*                                    http_invalid_timeout       = 4
*                                    OTHERS                     = 5 ).
*
*  CHECK sy-subrc = 0.
*
*  lo_get_response->receive(
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      OTHERS                     = 4 ).
*  CLEAR:lv_service,lt_data.
*  lv_service =  lo_get_response->response->get_cdata( ).
*
*  " Deserialize the JSON into the root structure
*  /ui2/cl_json=>deserialize(
*    EXPORTING
*      json        = lv_service
*      pretty_name = /ui2/cl_json=>pretty_mode-camel_case  " Handle camelCase
*    CHANGING
*      data        = lt_value
*  ).
*  IF sy-subrc = 0.
*    it_value =  lt_value.
*    LOOP AT it_value-value INTO DATA(_wa1).
*      wa_list_user-name = _wa1-identity-displayname.
*      APPEND wa_list_user TO it_list_user.
*      CLEAR wa_list_user.
*    ENDLOOP.
*
*    CLEAR: list.
*
*    LOOP AT it_list_user INTO DATA(wa_user1).
*      value-key = wa_user1-name.
*      APPEND value TO list.
*      CLEAR: wa_user1.
*    ENDLOOP.
*
*    SORT list.
*
*    DELETE ADJACENT DUPLICATES FROM list.
*    APPEND LINES OF list TO lista.
*  ENDIF.
*
*  FREE: lt_value.
*
*  DATA(get_users_pro) = |https://amaggi.visualstudio.com/_apis/projects/SAP/teams/d6caff30-7a0d-421a-b560-aafa2fa6092a/members?api-version=7.1|. "Os Ninjas - Projetos",
*
*  cl_http_client=>create_by_url(
*    EXPORTING
*      url                = get_users_pro
*    IMPORTING
*      client             = lo_get_response
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      OTHERS             = 4
*  ).
*
*
*  " TRY.
*  lo_get_response->request->set_header_field( name  = 'Content-Type' value = 'application/json; charset=utf-8' ).
*  lo_get_response->request->set_header_field( name  = 'Authorization' value = authenticate_api ).
*  lo_get_response->request->set_method( 'GET' ).
*  "lo_get_response->request->set_cdata( ).
*  lo_get_response->send( EXCEPTIONS http_communication_failure = 1
*                                    http_invalid_state         = 2
*                                    http_processing_failed     = 3
*                                    http_invalid_timeout       = 4
*                                    OTHERS                     = 5 ).
*
*  CHECK sy-subrc = 0.
*
*  lo_get_response->receive(
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      OTHERS                     = 4 ).
*  CLEAR:lv_service,lt_data.
*  lv_service =  lo_get_response->response->get_cdata( ).
*
*  " Deserialize the JSON into the root structure
*  /ui2/cl_json=>deserialize(
*    EXPORTING
*      json        = lv_service
*      pretty_name = /ui2/cl_json=>pretty_mode-camel_case  " Handle camelCase
*    CHANGING
*      data        = lt_value
*  ).
*  IF sy-subrc = 0.
*    it_value =  lt_value.
*    LOOP AT it_value-value INTO DATA(_wa2).
*      wa_list_user-name = _wa2-identity-displayname.
*      APPEND wa_list_user TO it_list_user.
*      CLEAR wa_list_user.
*    ENDLOOP.
*
*    CLEAR: list.
*
*    LOOP AT it_list_user INTO DATA(wa_user2).
*      value-key = wa_user2-name.
*      APPEND value TO list.
*      CLEAR: wa_user2.
*    ENDLOOP.
*
*    SORT list.
*
*    DELETE ADJACENT DUPLICATES FROM list.
*    APPEND LINES OF list TO lista.
*
*  ENDIF.
*
*  FREE: lt_value.
*
*  IF it_value IS NOT INITIAL.
*
*    SORT lista.
*    DELETE ADJACENT DUPLICATES FROM lista.
*
*    DATA wa_users TYPE zusers_devops.
*    DATA it_users TYPE STANDARD TABLE OF zusers_devops INITIAL SIZE 0.
*
*    LOOP AT lista ASSIGNING FIELD-SYMBOL(<fs_lista>).
*      wa_users-nome = <fs_lista>-key.
*     append wa_users to it_users.
*      CLEAR:wa_users.
*    ENDLOOP.
*
*          MODIFY zusers_devops FROM TABLE it_users.
*      COMMIT WORK.

**  DATA: lista  TYPE vrm_values.
**
**SELECT * FROM ZUSERS_DEVOPS INTO TABLE @DATA(IT_ZUSERS_DEVOPS).
**
**  LOOP AT IT_ZUSERS_DEVOPS ASSIGNING FIELD-SYMBOL(<FS_USERS>).
**vrm_values-
**  ENDLOOP.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = 'P_US01'
*        values          = lista
*      EXCEPTIONS
*        id_illegal_name = 0
*        OTHERS          = 0.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = 'P_US05'
*        values          = lista
*      EXCEPTIONS
*        id_illegal_name = 0
*        OTHERS          = 0.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = 'P_US06'
*        values          = lista
*      EXCEPTIONS
*        id_illegal_name = 0
*        OTHERS          = 0.
*
*  ENDIF.


ENDFORM.
