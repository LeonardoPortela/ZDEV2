FUNCTION zsdmf_enviar_xml_ecommerce.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     VALUE(I_ACTTAB) TYPE  J_1BNFE_ACTIVE OPTIONAL
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_erro TYPE flag.
  DATA lv_xml TYPE string.
  DATA lv_xstr TYPE xstring.
  DATA lv_key TYPE j_1b_nfe_access_key.
  DATA lv_chave TYPE string.
  DATA lv_ecom TYPE zsde_ecomm_id.
  DATA lv_estorno TYPE c.
  DATA ls_act TYPE j_1bnfe_active.

  ls_act = i_acttab.

  DATA lo_envio TYPE REF TO zcl_int_nf_ecomm_xml.

  IF i_docnum IS NOT INITIAL.

    SELECT SINGLE * FROM j_1bnfe_active
      INTO ls_act
        WHERE docnum = i_docnum.

  ENDIF.

  MOVE-CORRESPONDING ls_act TO lv_key.

  lv_chave = lv_key.

  IF ls_act IS INITIAL.

    PERFORM f_mensagem_insere
     TABLES et_return
      USING 'E' 'ZSD' '001'
            'chave não foi encontrada' '' '' ''.

    EXIT.

  ENDIF.

  IF NOT ( ls_act-action_requ = 'C' AND ls_act-docsta = '1' ).

    PERFORM f_mensagem_insere
     TABLES et_return
      USING 'E' 'ZSD' '001'
            'NFE não aprovada ainda' '' '' ''.

    EXIT.

  ENDIF.

  IF ls_act-cancel IS NOT INITIAL.
    lv_estorno = 'X'.
  ENDIF.

  PERFORM f_nf_valida_envio
    USING ls_act-docnum
 CHANGING lv_ecom.

  IF lv_ecom IS INITIAL.

    PERFORM f_mensagem_insere
     TABLES et_return
      USING 'E' 'ZSD' '001'
            'NF não é valida para envio' '' '' ''.

    EXIT.

  ENDIF.

  IF lv_estorno = space.

    TRY.

        CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
          EXPORTING
            i_docnum = i_docnum
            i_chave  = lv_chave
            i_tipo   = 'XML'
          IMPORTING
            out      = lv_xstr.

      CATCH zcx_doc_eletronico INTO DATA(lo_exc).

        PERFORM f_mensagem_insere
         TABLES et_return
          USING 'E' 'ZSD' '001'
                'xml nao encontrado' '' '' ''.

        EXIT.

    ENDTRY.

    IF lv_xstr IS INITIAL.

      PERFORM f_mensagem_insere
       TABLES et_return
        USING 'E' 'ZSD' '001'
              'xml nao encontrado' '' '' ''.

      EXIT.

    ENDIF.

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = lv_xstr
        im_encoding = '1100'
      IMPORTING
        ex_string   = lv_xml.

    IF lv_xml IS INITIAL.

      PERFORM f_mensagem_insere
       TABLES et_return
        USING 'E' 'ZSD' '001'
              'erro na conversao do XML' '' '' ''.

      EXIT.

    ENDIF.

  ENDIF.

  PERFORM f_debug_mode. "#apagar após testes

  TRY .

      CREATE OBJECT lo_envio.

      IF lv_estorno IS INITIAL.

        DATA(ls_integ) = lo_envio->criar_fatura( iv_ecomm = lv_ecom iv_xml = lv_xml ).

      ELSE.

        SELECT SINGLE nfenum FROM j_1bnfdoc INTO @DATA(lv_nfenum) WHERE docnum = @ls_act-docnum.

        ls_integ = lo_envio->estornar_fatura( iv_ecomm = lv_ecom iv_nfenum = lv_nfenum ).

      ENDIF.

      PERFORM f_get_integ_mess USING ls_integ CHANGING et_return[].

  ENDTRY.

ENDFUNCTION.
