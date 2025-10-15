*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 23/05/2013                                              &*
*& Descrição: Alterar caracteristica dos lotes                        &*
*& Transação: ZMM0046                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Report  ZMMR0026
*&
*&---------------------------------------------------------------------*
REPORT  zmmr0026 NO STANDARD PAGE HEADING LINE-SIZE 77.

** TABLES
**----------------------------------------------------------------------
TABLES: mch1, cabn, ausp.   "STANDARD

** CONSTANTS
**----------------------------------------------------------------------


** TYPES
**----------------------------------------------------------------------
TYPES:  BEGIN OF ty_mch1,
          matnr     TYPE mch1-matnr,
          charg     TYPE mch1-charg,
        END OF ty_mch1,

        BEGIN OF ty_msg,
          matnr     TYPE mch1-matnr,
          charg     TYPE mch1-charg,
          atnam     TYPE cabn-atnam,
          atwrt     TYPE ausp-atwrt,
          tipo      TYPE char1,
          msg       TYPE char40,
        END OF ty_msg.

** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: it_mch1       TYPE TABLE OF ty_mch1,
      it_obj_keys   TYPE TABLE OF bapi1003_object_keys,
      it_num        TYPE TABLE OF bapi1003_alloc_values_num,
      it_char       TYPE TABLE OF bapi1003_alloc_values_char,
      it_char_aux   TYPE TABLE OF bapi1003_alloc_values_char,
      it_curr       TYPE TABLE OF bapi1003_alloc_values_curr,
      it_return     TYPE TABLE OF bapiret2,
      it_msg        TYPE TABLE OF ty_msg.
*      it_ZMMT0025   TYPE TABLE OF zmmt0025.

** WORK AREAS
**----------------------------------------------------------------------
DATA: wa_mch1       TYPE ty_mch1,
      wa_obj_keys   TYPE bapi1003_object_keys,
      wa_char       TYPE bapi1003_alloc_values_char,
      wa_return     TYPE bapiret2,
      wa_msg        TYPE ty_msg,
      wa_zmmt0025   TYPE zmmt0025.

** VARIABLES
**----------------------------------------------------------------------
DATA: vl_num        TYPE bapi1003_key-classnum,
      vl_type       TYPE bapi1003_key-classtype,
      vl_table      TYPE bapi1003_key-objecttable,
      vl_key        TYPE bapi1003_key-object,
      vl_msg        TYPE char50.

** SELECTION SCREEN
**----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK one WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_matnr   FOR mch1-matnr      OBLIGATORY              NO INTERVALS,
  s_charg   FOR mch1-charg      OBLIGATORY              NO INTERVALS,
  s_atnam   FOR cabn-atnam      OBLIGATORY NO-EXTENSION NO INTERVALS,
  s_atwrt   FOR ausp-atwrt      OBLIGATORY NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK one.



*----------------------------------------------------------------------
* Evento: Start-of-selection
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM:  f_verificar_class,
            f_exibe_rel.


*&---------------------------------------------------------------------*
*&      Form  F_VERIFICAR_CLASS
*&---------------------------------------------------------------------*
*       Verificar se classificaçao informada existe
*----------------------------------------------------------------------*
FORM f_verificar_class.
  SELECT SINGLE * FROM zmmt0025 INTO wa_zmmt0025 WHERE atnam EQ s_atnam-low.
  IF sy-subrc <> 0.
    CONCATENATE 'Classificaçao "' s_atnam-low ' " incorreta.' INTO vl_msg.
    MESSAGE vl_msg TYPE 'I'.
  ELSE.
    PERFORM: f_seleciona_dados.
  ENDIF.
ENDFORM.                    " F_VERIFICAR_CLASS

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

** VERIFICAR SE MATERIAIS E LOTES EXISTE
  SELECT  matnr charg
  FROM mch1
    INTO TABLE it_mch1
  WHERE matnr IN s_matnr
    AND charg IN s_charg.

  CHECK it_mch1[] IS NOT INITIAL.

  PERFORM: f_busca_classificacao.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_CLASSIFICACAO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_busca_classificacao.
  vl_table  = 'MCH1'.
  vl_num    = 'FARDOES'.
  vl_type   = '023'.

  LOOP AT it_mch1 INTO wa_mch1.
    CLEAR:    vl_key, wa_obj_keys, wa_char.

    REFRESH:  it_obj_keys, it_num, it_char, it_curr, it_return, it_char_aux.

    wa_obj_keys-key_field = 'MATNR'.
    wa_obj_keys-value_int = wa_mch1-matnr.
    APPEND wa_obj_keys TO it_obj_keys.

    wa_obj_keys-key_field = 'CHARG'.
    wa_obj_keys-value_int = wa_mch1-charg.
    APPEND wa_obj_keys TO it_obj_keys.

** CONCATENAR MATERIAL E LOTE
    CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
      EXPORTING
        objecttable    = vl_table
      IMPORTING
        objectkey_conc = vl_key
      TABLES
        objectkeytable = it_obj_keys
        return         = it_return.

    IF sy-subrc IS INITIAL.
** VERIFICAR E BUSCAR CARACTERÍSTICAS EXISTENTES
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          objectkey       = vl_key
          objecttable     = vl_table
          classnum        = vl_num
          classtype       = vl_type
        TABLES
          allocvaluesnum  = it_num
          allocvalueschar = it_char
          allocvaluescurr = it_curr
          return          = it_return.

      IF sy-subrc = 0.
        REFRESH: it_return.
        PERFORM: f_alterar_caracteristica.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_BUSCA_CLASSIFICACAO

*&---------------------------------------------------------------------*
*&      Form  F_ALTERA_CARACTERISTICA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_alterar_caracteristica.
  READ TABLE it_char INTO wa_char WITH KEY charact = s_atnam-low.
** SE CARACTERÍSTICA EXISTIR, ALTERAR
  IF sy-subrc = 0.
    wa_char-value_char    = s_atwrt-low.
    wa_char-value_neutral = s_atwrt-low.
    APPEND wa_char TO it_char_aux.

    CLEAR: wa_char.
    LOOP AT it_char INTO wa_char WHERE charact NE s_atnam-low.
      APPEND wa_char TO it_char_aux.
    ENDLOOP.
  ELSE.
** SE CARACTERÍSTICA NÃO EXISTIR, ADICIONAR
    wa_char-charact       = wa_zmmt0025-atnam.    "s_atnam-low.
    wa_char-value_char    = s_atwrt-low.
*    wa_char-INHERITED     =
*    wa_char-INSTANCE      =
    wa_char-value_neutral = s_atwrt-low.
    wa_char-charact_descr = wa_zmmt0025-atnam.    "s_atnam-low.
    APPEND wa_char TO it_char_aux.

    CLEAR: wa_char.
    LOOP AT it_char INTO wa_char WHERE charact NE s_atnam-low.
      APPEND wa_char TO it_char_aux.
    ENDLOOP.
  ENDIF.

  IF NOT it_char_aux[] IS  INITIAL.
** ALTERAR CARACTERÍSTICA
    CALL FUNCTION 'BAPI_OBJCL_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        objectkey          = vl_key
        objecttable        = vl_table
        classnum           = vl_num
        classtype          = vl_type
      TABLES
        allocvaluesnumnew  = it_num
        allocvaluescharnew = it_char_aux
        allocvaluescurrnew = it_curr
        return             = it_return.

    DELETE it_return WHERE ( type NE 'E' AND type NE 'S' ).
    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.                    " F_ALTERA_CARACTERISTICA

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_MSG
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_monta_msg.
** MONTAR MSG DE SUCESSO E ERRO
  LOOP AT it_return INTO wa_return.
    wa_msg-matnr  = wa_mch1-matnr.
    wa_msg-charg  = wa_mch1-charg.
*    wa_msg-atnam  = wa_return-satnam-low.
*    wa_msg-atwrt  = wa_return-s_atwrt-low.
    wa_msg-tipo   = wa_return-type.
    wa_msg-msg    = wa_return-message.

    APPEND wa_msg TO it_msg.
  ENDLOOP.

ENDFORM.                    " F_MONTA_MSG

*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_REL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_exibe_rel.
  IF it_msg[] IS NOT INITIAL.
    DATA: vl_color  TYPE i.

    LOOP AT it_msg INTO wa_msg.
      AT FIRST.
        vl_color = 1.
        FORMAT COLOR 1.

        ULINE.
        WRITE:  sy-vline, (10)  'MATERIAL',
                sy-vline, (10)  'LOTE',
                sy-vline, (04)  'TIPO',
                sy-vline, (40)  'MENSAGEM',
                sy-vline.
        ULINE.
      ENDAT.

      " LOOP----------------------------------------------------
      IF vl_color = 1.
        FORMAT COLOR 2.
        vl_color = 2.
      ELSE.
        FORMAT COLOR 4.
        vl_color = 1.
      ENDIF.

      WRITE: /  sy-vline, (10)  wa_msg-matnr,
                sy-vline, (10)  wa_msg-charg,
                sy-vline, (04)  wa_msg-tipo,
                sy-vline, (40)  wa_msg-msg,
                sy-vline.
      " LOOP----------------------------------------------------
    ENDLOOP.
    ULINE.
  ENDIF.
ENDFORM.                    " F_EXIBE_REL
