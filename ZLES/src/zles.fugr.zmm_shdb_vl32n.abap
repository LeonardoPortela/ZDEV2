FUNCTION zmm_shdb_vl32n.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_VBELN) TYPE  LIKP-VBELN
*"  EXPORTING
*"     VALUE(ERROR_REVERSE_GOODS_ISSUE) TYPE  CHAR1
*"  TABLES
*"      T_MESG STRUCTURE  MESG
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_msg,
           tcode   TYPE bdcmsgcoll-tcode,
           dyname  TYPE bdcmsgcoll-dyname,
           dynumb  TYPE bdcmsgcoll-dynumb,
           msgtyp  TYPE bdcmsgcoll-msgtyp,
           msgspra TYPE bdcmsgcoll-msgspra,
           msgid   TYPE bdcmsgcoll-msgid,
           msgnr   TYPE bdcmsgcoll-msgnr,
           msgv1   TYPE bdcmsgcoll-msgv1,
           msgv2   TYPE bdcmsgcoll-msgv2,
           msgv3   TYPE bdcmsgcoll-msgv3,
           msgv4   TYPE bdcmsgcoll-msgv4,
           env     TYPE bdcmsgcoll-env,
           fldname TYPE bdcmsgcoll-fldname,
         END OF ty_msg.

* Declaração de Tabela BDCDATA para Batch-Imput
  TYPES: BEGIN OF ty_bdcdata,
           program  TYPE bdcdata-program,
           dynpro   TYPE bdcdata-dynpro,
           dynbegin TYPE bdcdata-dynbegin,
           fnam     TYPE bdcdata-fnam,
           fval     TYPE bdcdata-fval,
         END OF ty_bdcdata.

  DATA: vl_mode    TYPE c,
        vg_return  TYPE string,
        it_bdcdata TYPE TABLE OF ty_bdcdata,
        wa_bdcdata TYPE ty_bdcdata,
        w_mesg     TYPE mesg,
        it_msg     TYPE TABLE OF ty_msg.


  MOVE: 'SAPMV50A'       TO wa_bdcdata-program,
        '4104'           TO wa_bdcdata-dynpro,
        'X'              TO wa_bdcdata-dynbegin.
  APPEND wa_bdcdata      TO it_bdcdata.
  CLEAR wa_bdcdata.

  MOVE: 'BDC_OKCODE'     TO wa_bdcdata-fnam,
        '=ENT2'          TO wa_bdcdata-fval.
  APPEND wa_bdcdata      TO it_bdcdata.
  CLEAR wa_bdcdata.

  MOVE: 'LIKP-VBELN'     TO wa_bdcdata-fnam,
        p_vbeln          TO wa_bdcdata-fval.
  APPEND wa_bdcdata      TO it_bdcdata.
  CLEAR wa_bdcdata.

  MOVE: 'SAPMV50A'       TO wa_bdcdata-program,
         '1000'          TO wa_bdcdata-dynpro,
         'X'             TO wa_bdcdata-dynbegin.
  APPEND wa_bdcdata      TO it_bdcdata.
  CLEAR wa_bdcdata.

  MOVE: 'BDC_OKCODE'     TO wa_bdcdata-fnam,
        '/ELOES_T'       TO wa_bdcdata-fval.
  APPEND wa_bdcdata      TO it_bdcdata.
  CLEAR wa_bdcdata.

* Carregar Transaçãlo
  FREE: it_msg.

  vl_mode = 'N'.

  CALL TRANSACTION 'VL32N'
     USING it_bdcdata
     MODE   vl_mode
     UPDATE 'S'
     MESSAGES INTO it_msg.

  LOOP AT it_msg INTO DATA(wa_msg).

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = wa_msg-msgid
        msg_no                 = wa_msg-msgnr
        msg_var1               = wa_msg-msgv1(50)
        msg_var2               = wa_msg-msgv2(50)
        msg_var3               = wa_msg-msgv3(50)
        msg_var4               = wa_msg-msgv4(50)
      IMPORTING
        msg_text               = vg_return
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    MOVE-CORRESPONDING wa_msg TO w_mesg.
    w_mesg-msgty = wa_msg-msgtyp.
    w_mesg-text  = vg_return.
    APPEND w_mesg to t_mesg.

  ENDLOOP.


ENDFUNCTION.
