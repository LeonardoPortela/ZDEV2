class ZCL_IM_BADI_VALIDA_BP definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BUPA_FURTHER_CHECKS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BADI_VALIDA_BP IMPLEMENTATION.


  METHOD if_ex_bupa_further_checks~check_central.

    TYPES: BEGIN OF bptax_disptype.
             INCLUDE TYPE dfkkbptaxnum.
    TYPES:   xmark TYPE boole-boole,
           END   OF bptax_disptype.

    TYPES: bptax_dispttype TYPE TABLE OF bptax_disptype.

    DATA: ls_but000_forn   TYPE but000,  "*-#155327-16.10.2024-JT-inicio
          ls_but000_clie   TYPE but000,  "*-#155327-16.10.2024-JT-inicio
          ls_but000        TYPE but000,  "*-CS2021000253-#149089-07.01.2025-JT
          ls_lfa1          TYPE lfa1,
          ls_lfb1          TYPE lfb1,
          ls_lfm1          TYPE lfm1,
          ls_knb1          TYPE knb1,
          ls_knvv          TYPE knvv,
          ls_knbk          TYPE knbk,
          ls_set_role      TYPE char20,
          ls_tipo_parceiro TYPE char20,
          lc_lifnr         TYPE lfa1-lifnr,
          lc_kunnr         TYPE kna1-kunnr,
          t_lfb1           TYPE TABLE OF lfb1,
          t_lfm1           TYPE TABLE OF lfm1,
          t_lfbk           TYPE TABLE OF lfbk,
          t_lfb5           TYPE TABLE OF lfb5,
          t_lfza           TYPE TABLE OF lfza,
          t_lfbw           TYPE TABLE OF lfbw,
          t_lfas           TYPE TABLE OF lfas,
          t_lfat           TYPE TABLE OF lfat,
          t_lflr           TYPE TABLE OF lflr,
          t_lfm2           TYPE TABLE OF lfm2,
          t_wyt1           TYPE TABLE OF wyt1,
          t_wyt1t          TYPE TABLE OF wyt1t,
          t_wyt3           TYPE TABLE OF wyt3,
          ls_kna1          TYPE kna1,
          ls_knvk          TYPE knvk,
          t_knb1           TYPE TABLE OF knb1,
          t_knbw           TYPE TABLE OF knbw,
          t_knvi           TYPE TABLE OF knvi,
          t_knvv           TYPE TABLE OF knvv,
          t_knbk           TYPE TABLE OF knbk,
          ls_bapi_ret      TYPE bapiret2,
          gt_bptax         TYPE bptax_dispttype,
          gw_bptax         TYPE bptax_disptype,
          ls_addr2_data    TYPE addr2_data.

    FIELD-SYMBOLS: <fs_role>   TYPE any,
                   <fs_but000> TYPE any,   "*-#155327-16.10.2024-JT-inicio
                   <fs_lfa1>   TYPE any,
                   <fs_addr>   TYPE any,
                   <fs_lfb1>   TYPE any,
                   <fs_lfm1>   TYPE any,
                   <fs_lfbk>   TYPE ANY TABLE,
                   <fs_kna1>   TYPE any,
                   <fs_knb1>   TYPE any,
                   <fs_knvv>   TYPE any,
                   <fs_knbk>   TYPE ANY TABLE,
                   <fs_table>  TYPE ANY TABLE.

*------------------------------
*-- dados recebidos via BAPI
*------------------------------
    CALL FUNCTION 'ZLES_GET_DATA_BP'
      IMPORTING
        es_tipo_parceiro = ls_tipo_parceiro
        es_but000        = ls_but000   "*-CS2021000253-#149089-07.01.2025-JT
        es_lfa1          = ls_lfa1
        et_lfb1          = t_lfb1
        et_lfbk          = t_lfbk
        et_lfbw          = t_lfbw
        et_lfm1          = t_lfm1
        es_kna1          = ls_kna1
        es_knvk          = ls_knvk
        et_knb1          = t_knb1
        et_knbw          = t_knbw
        et_knvi          = t_knvi
        et_knvv          = t_knvv.

*------------------------------
*-- chamada via BAPI
*------------------------------
    IF ls_tipo_parceiro IS NOT INITIAL.
      ls_set_role = ls_tipo_parceiro.

      CASE ls_tipo_parceiro.
        WHEN 'FORNECEDOR'.
          READ TABLE t_lfb1 INTO ls_lfb1 INDEX 1.
          READ TABLE t_lfm1 INTO ls_lfm1 INDEX 1.
          ls_but000_forn       = ls_but000.  "*-CS2021000253-#149089-07.01.2025-JT
        WHEN 'CLIENTE'.
          READ TABLE t_knb1 INTO ls_knb1 INDEX 1.
          READ TABLE t_knvv INTO ls_knvv INDEX 1.
          ls_but000_clie       = ls_but000.  "*-CS2021000253-#149089-07.01.2025-JT
      ENDCASE.

    ELSE.
*------------------------------
*---- chamada via BP
*------------------------------
      ASSIGN ('(SAPLBUPA_DIALOG_JOEL)BUS_JOEL_MAIN-PARTNER_ROLE') TO <fs_role>.
      CHECK sy-subrc = 0.

*-----------------------------------------------------------
*---- Fornecedor ou geral
*-----------------------------------------------------------
      CASE <fs_role>(4).
        WHEN 'FLVN' OR "=======Fornecedor
             '0000' .  "=======Dados GErais
          ls_set_role = 'FORNECEDOR'.

          IF <fs_role>(4) = '0000'.
            ls_set_role = 'GERAL'.
          ENDIF.

*-#156755-31.10.2024-JT-inicio
          SELECT SINGLE supplier
            INTO lc_lifnr
            FROM ibpsupplier
           WHERE businesspartner = iv_partner.

          IF sy-subrc <> 0.
            lc_lifnr = iv_partner.
          ENDIF.
*-#156755-31.10.2024-JT-inicio

*-#155327-16.10.2024-JT-inicio
*------------------------------
*-------- set BUT000
*------------------------------
          ASSIGN ('(SAPLBUD0)BUT000')   TO <fs_but000>.
          IF sy-subrc = 0 AND <fs_but000> IS NOT INITIAL.
            ls_but000_forn = <fs_but000>.
          ENDIF.
*-#155327-16.10.2024-JT-fim

*------------------------------
*-------- set lfa1
*------------------------------
          ASSIGN ('(SAPLSZA7)ADDR2_DATA')   TO <fs_addr>.
          IF sy-subrc = 0 AND <fs_addr> IS NOT INITIAL.
            ls_addr2_data = <fs_addr>.
          ENDIF.

          ASSIGN ('(SAPLBUPA_BUTX_DIALOG)GT_BPTAX[]')   TO <fs_table>[].
          IF sy-subrc = 0 AND <fs_table>[] IS NOT INITIAL.
            gt_bptax[] = <fs_table>.
          ENDIF.

          ASSIGN ('(SAPLCVI_FS_UI_VENDOR)gs_lfa1')      TO <fs_lfa1>.
          IF sy-subrc = 0 AND <fs_lfa1> IS NOT INITIAL.
            ls_lfa1       = <fs_lfa1>.
            ls_lfa1-lifnr = lc_lifnr. "iv_partner.  "*-#156755-31.10.2024-JT-inicio
            ls_lfa1-regio = ls_addr2_data-region.

*-CS2024000622-26.07.2024-JT-#146685-inicio
            IF ls_lfa1-regio IS INITIAL.
              SELECT SINGLE region
                INTO @DATA(_regio_for)
                FROM adrc
               WHERE addrnumber = @ls_lfa1-adrnr
                 AND date_to   >= @sy-datum.
              IF sy-subrc = 0.
                ls_lfa1-regio = _regio_for.
              ENDIF.
            ENDIF.
*-CS2024000622-26.07.2024-JT-#146685-fim
          ELSE.
            SELECT SINGLE *
              FROM lfa1
              INTO ls_lfa1
             WHERE lifnr = lc_lifnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR1'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_lfa1-stcd1 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'AR1A'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL.
            ls_lfa1-stcd1 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR2'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL.
            ls_lfa1-stcd2 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'AR1B'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL.
            ls_lfa1-stcd2 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR3'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL.
            ls_lfa1-stcd3 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR4'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL.
            ls_lfa1-stcd4 = gw_bptax-taxnum.
          ENDIF.

*------------------------------
*-------- set lfb1
*------------------------------
          ASSIGN ('(SAPLCVI_FS_UI_VENDOR_CC)gs_lfb1')   TO <fs_lfb1>.
          IF sy-subrc = 0 AND <fs_lfb1> IS NOT INITIAL..
            ls_lfb1 = <fs_lfb1>.
          ELSE.
            SELECT SINGLE *
              FROM lfb1
              INTO ls_lfb1
             WHERE lifnr = lc_lifnr. "iv_partner.  "*-#156755-31.10.2024-JT-inicio
          ENDIF.

*------------------------------
*-------- set lfm1
*------------------------------
          ASSIGN ('(SAPLCVI_FS_UI_VENDOR_PORG)gs_lfm1') TO <fs_lfm1>.
          IF sy-subrc = 0 AND <fs_lfm1> IS NOT INITIAL..
            ls_lfm1 = <fs_lfm1>.
          ELSE.
            SELECT SINGLE *
              FROM lfm1
              INTO ls_lfm1
             WHERE lifnr = lc_lifnr. "iv_partner.  "*-#156755-31.10.2024-JT-inicio
          ENDIF.

*------------------------------
*-------- set lfbk
*------------------------------
          ASSIGN ('(SAPLBUD0)GT_BUT0BK[]')             TO <fs_lfbk>.
          IF sy-subrc = 0 AND <fs_lfbk> IS NOT INITIAL.
            MOVE-CORRESPONDING <fs_lfbk>[] TO t_lfbk[].
          ELSE.
            SELECT *
              FROM lfbk
              INTO TABLE t_lfbk
             WHERE lifnr = lc_lifnr. "iv_partner.  "*-#156755-31.10.2024-JT-inicio
          ENDIF.
      ENDCASE.

*-----------------------------------------------------------
*---- Cliente ou geral
*-----------------------------------------------------------
      CASE <fs_role>(4).
        WHEN 'FLCU' OR "=======Cliente
             '0000' .  "=======Dados GErais
          ls_set_role = 'CLIENTE'.

          IF <fs_role>(4) = '0000'.
            ls_set_role = 'GERAL'.
          ENDIF.

*-#156755-31.10.2024-JT-inicio
          SELECT SINGLE customer
            INTO lc_kunnr
            FROM ibupacustomer
           WHERE businesspartner = iv_partner.

          IF sy-subrc <> 0.
            lc_kunnr = iv_partner.
          ENDIF.
*-#156755-31.10.2024-JT-inicio

*-#155327-16.10.2024-JT-inicio
*------------------------------
*-------- set BUT000
*------------------------------
          ASSIGN ('(SAPLBUD0)BUT000')   TO <fs_but000>.
          IF sy-subrc = 0 AND <fs_but000> IS NOT INITIAL.
            ls_but000_clie = <fs_but000>.
          ENDIF.
*-#155327-16.10.2024-JT-fim

*------------------------------
*-------- set kna1
*------------------------------
          ASSIGN ('(SAPLSZA7)ADDR2_DATA')   TO <fs_addr>.
          IF sy-subrc = 0 AND <fs_addr> IS NOT INITIAL.
            ls_addr2_data = <fs_addr>.
          ENDIF.

          ASSIGN ('(SAPLBUPA_BUTX_DIALOG)GT_BPTAX[]')   TO <fs_table>[].
          IF sy-subrc = 0 AND <fs_table>[] IS NOT INITIAL.
            gt_bptax[] = <fs_table>.
          ENDIF.

          ASSIGN ('(SAPLCVI_FS_UI_CUSTOMER)gs_kna1')    TO <fs_kna1>.
          IF sy-subrc = 0 AND <fs_kna1> IS NOT INITIAL.
            ls_kna1       = <fs_kna1>.
            ls_kna1-kunnr = lc_kunnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
            ls_kna1-regio = ls_addr2_data-region.

*-CS2024000622-26.07.2024-JT-#146685-inicio
            IF ls_kna1-regio IS INITIAL.
              SELECT SINGLE region
                INTO @DATA(_regio_cli)
                FROM adrc
               WHERE addrnumber = @ls_kna1-adrnr
                 AND date_to   >= @sy-datum.
              IF sy-subrc = 0.
                ls_kna1-regio = _regio_cli.
              ENDIF.
*-CS2024000622-26.07.2024-JT-#146685-fim
            ENDIF.
          ELSE.
            SELECT SINGLE *
              FROM kna1
              INTO ls_kna1
             WHERE kunnr = lc_kunnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR1'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd1 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'AR1A'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd1 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR2'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd2 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'AR1B'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd2 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR3'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd3 = gw_bptax-taxnum.
          ENDIF.

          READ TABLE gt_bptax INTO gw_bptax WITH KEY taxtype = 'BR4'.
          IF sy-subrc = 0 AND gw_bptax-taxnum IS NOT INITIAL..
            ls_kna1-stcd4 = gw_bptax-taxnum.
          ENDIF.

*------------------------------
*-------- set knb1
*------------------------------
          ASSIGN ('(SAPLCVI_FS_UI_CUSTOMER_CC)gs_knb1')    TO <fs_knb1>.
          IF sy-subrc = 0 AND <fs_knb1> IS NOT INITIAL.
            ls_knb1 = <fs_knb1>.
          ELSE.
            SELECT SINGLE *
              FROM knb1
              INTO ls_knb1
             WHERE kunnr = lc_kunnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
          ENDIF.

*------------------------------
*-------- set knbk
*------------------------------
          ASSIGN ('(SAPLBUD0)GT_BUT0BK[]')             TO <fs_knbk>.
          IF sy-subrc = 0 AND <fs_knbk> IS NOT INITIAL.
            MOVE-CORRESPONDING <fs_knbk>[] TO t_knbk[].
          ELSE.
            SELECT *
              FROM knbk
              INTO TABLE t_knbk
             WHERE kunnr = lc_kunnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
          ENDIF.

*------------------------------
*-------- set knvv
*------------------------------
          ASSIGN ('(SAPLCVI_FS_UI_CUSTOMER_SALES)gs_knvv')    TO <fs_knvv>.
          IF sy-subrc = 0 AND <fs_knvv> IS NOT INITIAL.
            ls_knvv = <fs_knvv>.
          ELSE.
            SELECT SINGLE *
              FROM knvv
              INTO ls_knvv
             WHERE kunnr = lc_kunnr. "iv_partner. "*-#156755-31.10.2024-JT-inicio
          ENDIF.
      ENDCASE.
    ENDIF.

*-----------------------------------------------------------
*-- executa validacoes
*-- Fornecedor ou geral
*-----------------------------------------------------------
    CASE ls_set_role.
      WHEN 'FORNECEDOR' OR 'GERAL'.
        CALL FUNCTION 'ZEXIT_SAPMF02K_001'
          EXPORTING
            i_but000 = ls_but000_forn  "*-#155327-16.10.2024-JT-inicio
            i_lfa1   = ls_lfa1
            i_lfb1   = ls_lfb1
            i_lfm1   = ls_lfm1
          TABLES
            t_lfbk   = t_lfbk
            t_lfb5   = t_lfb5
            t_lfza   = t_lfza
            t_lfbw   = t_lfbw
            t_lfas   = t_lfas
            t_lfat   = t_lfat
            t_lflr   = t_lflr
            t_lfm2   = t_lfm2
            t_wyt1   = t_wyt1
            t_wyt1t  = t_wyt1t
            t_wyt3   = t_wyt3
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF sy-subrc <> 0.
*-------- retorno erro
          ls_bapi_ret-type       = sy-msgty.
          ls_bapi_ret-id         = sy-msgid.
          ls_bapi_ret-number     = sy-msgno.
          ls_bapi_ret-message_v1 = sy-msgv1.
          ls_bapi_ret-message_v2 = sy-msgv2.
          ls_bapi_ret-message_v3 = sy-msgv3.
          ls_bapi_ret-message_v4 = sy-msgv4.
          APPEND ls_bapi_ret    TO et_return.
          RETURN.
        ENDIF.
    ENDCASE.

*-----------------------------------------------------------
*-- Cliente ou geral
*-----------------------------------------------------------
    CASE ls_set_role.
      WHEN 'CLIENTE' OR 'GERAL'.
        CALL FUNCTION 'ZEXIT_SAPMF02D_001'
          EXPORTING
            i_but000 = ls_but000_clie  "*-#155327-16.10.2024-JT-inicio
            i_kna1   = ls_kna1
            i_knb1   = ls_knb1
            i_knvv   = ls_knvv
          TABLES
            t_knbk   = t_knbk
            t_knbw   = t_knbw
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF sy-subrc <> 0.
*-------- retorno erro
          ls_bapi_ret-type       = sy-msgty.
          ls_bapi_ret-id         = sy-msgid.
          ls_bapi_ret-number     = sy-msgno.
          ls_bapi_ret-message_v1 = sy-msgv1.
          ls_bapi_ret-message_v2 = sy-msgv2.
          ls_bapi_ret-message_v3 = sy-msgv3.
          ls_bapi_ret-message_v4 = sy-msgv4.
          APPEND ls_bapi_ret    TO et_return.
          RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
