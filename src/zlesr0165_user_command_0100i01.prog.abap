*----------------------------------------------------------------------*
***INCLUDE ZLESR0165_USER_COMMAND_0100I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_check_contrato INPUT.

  PERFORM f_check_contrato.

ENDMODULE.

MODULE f_check_ajuste INPUT.

  PERFORM f_check_ajuste.

ENDMODULE.

MODULE f_check_peso_descarga INPUT.

  PERFORM f_check_peso_descarga.

ENDMODULE.

MODULE f_help_contrato INPUT.

  DATA: t_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        t_mapping    TYPE STANDARD TABLE OF dselc,
        w_mapping    TYPE dselc,
        t_fieldtab   TYPE TABLE OF dfies,
        w_fieldtab   TYPE dfies.

  DATA: BEGIN OF t_docs OCCURS 0,
          nucontrato TYPE zcte_ciot-nucontrato,
          cd_ciot    TYPE zcte_ciot-cd_ciot,
          nr_ciot    TYPE zcte_ciot-nr_ciot,
          ct_nome    TYPE zcte_ciot-ct_nome,
          ct_cnpj    TYPE zcte_ciot-ct_cnpj,
        END OF t_docs.

  FREE: t_mapping, t_fieldtab.

  SELECT nucontrato cd_ciot nr_ciot ct_nome ct_cnpj
    FROM zcte_ciot
    INTO TABLE t_docs
   WHERE st_ciot   IN ('5','6')
     AND cancelado  = abap_off.

  SORT t_docs BY nucontrato.

  w_mapping-fldname   = 'F0001'.
  w_mapping-dyfldname = 'NUCONTRATO'.
  APPEND w_mapping   TO t_mapping.
  w_mapping-fldname   = 'F0002'.
  w_mapping-dyfldname = 'CD_CIOT'.
  APPEND w_mapping   TO t_mapping.
  w_mapping-fldname   = 'F0003'.
  w_mapping-dyfldname = 'NR_CIOT'.
  APPEND w_mapping   TO t_mapping.
  w_mapping-fldname   = 'F0004'.
  w_mapping-dyfldname = 'CT_NOME'.
  APPEND w_mapping   TO t_mapping.
  w_mapping-fldname   = 'F0005'.
  w_mapping-dyfldname = 'CT_CNPJ'.
  APPEND w_mapping   TO t_mapping.

  PERFORM f_fieldinfo_get USING 'ZCTE_CIOT'
                                'NUCONTRATO'
                       CHANGING w_fieldtab.

  w_fieldtab-tabname   = 'T_DOCS'.
  w_fieldtab-fieldname = 'NUCONTRATO'.
  w_fieldtab-position  = 1.
  w_fieldtab-offset    = 0.
  w_fieldtab-reptext   = 'Contrato'.
  APPEND w_fieldtab   TO t_fieldtab.

  PERFORM f_fieldinfo_get USING 'ZCTE_CIOT'
                                'CD_CIOT'
                       CHANGING w_fieldtab.

  w_fieldtab-tabname   = 'T_DOCS'.
  w_fieldtab-fieldname = 'CD_CIOT'.
  w_fieldtab-position  = 2.
  w_fieldtab-offset    = 24.
  w_fieldtab-reptext   = 'CD Ciot'.
  APPEND w_fieldtab   TO t_fieldtab.

  PERFORM f_fieldinfo_get USING 'ZCTE_CIOT'
                                'NR_CIOT'
                       CHANGING w_fieldtab.

  w_fieldtab-tabname   = 'T_DOCS'.
  w_fieldtab-fieldname = 'NR_CIOT'.
  w_fieldtab-position  = 3.
  w_fieldtab-offset    = 48.
  w_fieldtab-reptext   = 'NR Ciot'.
  APPEND w_fieldtab   TO t_fieldtab.

  PERFORM f_fieldinfo_get USING 'ZCTE_CIOT'
                                'CT_NOME'
                       CHANGING w_fieldtab.

  w_fieldtab-tabname   = 'T_DOCS'.
  w_fieldtab-fieldname = 'CT_NOME'.
  w_fieldtab-position  = 4.
  w_fieldtab-offset    = 72.
  w_fieldtab-reptext   = 'Nome'.
  APPEND w_fieldtab   TO t_fieldtab.

  PERFORM f_fieldinfo_get USING 'ZCTE_CIOT'
                                'CT_CNPJ'
                       CHANGING w_fieldtab.

  w_fieldtab-tabname   = 'T_DOCS'.
  w_fieldtab-fieldname = 'CT_CNPJ'.
  w_fieldtab-position  = 5.
  w_fieldtab-offset    = 192.
  w_fieldtab-reptext   = 'CNPJ'.
  APPEND w_fieldtab   TO t_fieldtab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NUCONTRATO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZCARGA_CTE_TIP-NUCONTRATO'
      value_org       = 'S'
    TABLES
      field_tab       = t_fieldtab
      value_tab       = t_docs
      return_tab      = t_return_tab
      dynpfld_mapping = t_mapping.

ENDMODULE.

MODULE f_help_ajuste INPUT.

  DATA: BEGIN OF t_ajuste OCCURS 0,
          chvid    TYPE zlest0025-chvid,
          deschvid TYPE zlest0025-deschvid,
        END OF t_ajuste.

  FREE: t_mapping, t_fieldtab.

  SELECT chvid deschvid
    FROM zlest0025
    INTO TABLE t_ajuste
   WHERE ctlgchavid  = 'AP'
     AND bl          = '2'.

  SORT t_ajuste BY chvid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CHVID'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'ZLEST0025-CHVID'
      value_org   = 'S'
    TABLES
*     field_tab   = t_fieldtab
      value_tab   = t_ajuste
      return_tab  = t_return_tab.
*     dynpfld_mapping = t_mapping.

ENDMODULE.

MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&NOVO'.
      lv_tela = COND #( WHEN lv_erro = abap_false THEN 'N' ELSE 'C' ).
      IF lv_tela = 'N'.
        PERFORM f_lock_contrato USING 'B' zcarga_cte_tip-nucontrato.
      ENDIF.
*     PERFORM f_clear_dados.
*     PERFORM f_selecao_dados CHANGING lv_erro.
*     IF lv_erro = abap_false.
*       PERFORM f_montar_dados.
*     ENDIF.

    WHEN '&CONSULTA'.
      lv_tela = 'C'.
      PERFORM f_lock_contrato USING 'D' zcarga_cte_tip-nucontrato.
      PERFORM f_clear_dados.
      PERFORM f_selecao_dados  CHANGING lv_erro.
      IF lv_erro = abap_false.
        PERFORM f_montar_dados.
      ENDIF.

    WHEN 'BTN_AJUSTE'.
      PERFORM f_executa_tela       USING zlest0025-chvid.

    WHEN 'SALVAR'.
      PERFORM f_validar_dados      USING zlest0025-chvid
                                CHANGING lv_erro.
      IF lv_erro = abap_false.
        PERFORM f_gravar_dados  CHANGING lv_erro.
        IF lv_erro = abap_false.
          lv_tela = 'C'.
          PERFORM f_lock_contrato USING 'D' zcarga_cte_tip-nucontrato.
          PERFORM f_clear_dados.
          PERFORM f_selecao_dados CHANGING lv_erro.
          IF lv_erro = abap_false.
            PERFORM f_montar_dados.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      PERFORM f_lock_contrato USING 'D' zcarga_cte_tip-nucontrato.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.

FORM f_fieldinfo_get USING p_tabname
                           p_fieldname
                  CHANGING p_field_tab.

  CLEAR p_field_tab.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
      fieldname      = p_fieldname
      lfieldname     = p_fieldname
    IMPORTING
      dfies_wa       = p_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

ENDFORM.
