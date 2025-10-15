class ZCL_MATERIAL_DESTINACAO_DEVO definition
  public
  inheriting from ZCL_MATERIAL_DESTINACAO
  create public .

public section.

  methods CONSTRUCTOR .

  methods ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG
    redefinition .
  methods ZIF_MATERIAL_DESTINACAO~SET_GERAR_MOVIMENTO
    redefinition .
  methods ZIF_MATERIAL_DESTINACAO~SET_VALIDAR
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MATERIAL_DESTINACAO_DEVO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    SUPER->CONSTRUCTOR( ).

    ME->ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG( ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG.

    R_IF_MATERIAL_DESTINACAO = SUPER->ZIF_MATERIAL_DESTINACAO~SET_DEFAULT_CONFIG( ).

    ME->ZIF_MATERIAL_DESTINACAO~AT_TP_DESTINACAO    = ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_DEVOLUCAO.
    ME->ZIF_MATERIAL_DESTINACAO~AT_TIPO_MOVIMENTO   = '122'.
    ME->ZIF_MATERIAL_DESTINACAO~AT_MOTIVO_MOVIMENTO = '0002'.

  ENDMETHOD.


  METHOD zif_material_destinacao~set_gerar_movimento.

    r_if_material_destinacao =
      me->zif_material_destinacao~set_gerar_nota_devolucao( EXPORTING i_gerar_via_job = i_gerar_via_job  "*-CS2025000249-27.05.2025-#175255-JT
                                                            IMPORTING e_belnr_dev     = e_belnr_dev e_gjahr_dev  = e_gjahr_dev e_docnum_dev = e_docnum_dev ).

    CHECK me->zif_material_destinacao~at_zmmt0114-docnum_dev IS NOT INITIAL.

    TRY .

        zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_material_destinacao~at_zmmt0114-docnum_dev
          )->set_registro( EXPORTING i_docnum = me->zif_material_destinacao~at_zmmt0114-docnum_dev i_sem_bloqueio = abap_true
          )->get_ck_autorizado_uso(
          ).

      CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).    " .

        "*-CS2025000249-27.05.2025-#175255-JT-inicio
        super->zif_material_destinacao~set_gerar_movimento(
         EXPORTING
           i_dt_movimento           = i_dt_movimento
         IMPORTING
           e_gerou                  = e_gerou
           e_mblnr                  = e_mblnr
           e_mjahr                  = e_mjahr
           e_docnum                 = e_docnum
         RECEIVING
           r_if_material_destinacao = r_if_material_destinacao ).
        "*-CS2025000249-27.05.2025-#175255-JT-fim

        RAISE EXCEPTION TYPE zcx_material_destinacao
          EXPORTING
            textid = VALUE #( msgid = ex_doc_eletronico->msgid
                              msgno = ex_doc_eletronico->msgno
                              attr1 = ex_doc_eletronico->msgv1
                              attr2 = ex_doc_eletronico->msgv2
                              attr3 = ex_doc_eletronico->msgv3
                              attr4 = ex_doc_eletronico->msgv4 )
            msgid  = ex_doc_eletronico->msgid
            msgno  = ex_doc_eletronico->msgno
            msgv1  = ex_doc_eletronico->msgv1
            msgv2  = ex_doc_eletronico->msgv2
            msgv3  = ex_doc_eletronico->msgv3
            msgv4  = ex_doc_eletronico->msgv4
            msgty  = 'E'.

    ENDTRY.

    super->zif_material_destinacao~set_gerar_movimento(
     EXPORTING
       i_dt_movimento           = i_dt_movimento
     IMPORTING
       e_gerou                  = e_gerou
       e_mblnr                  = e_mblnr
       e_mjahr                  = e_mjahr
       e_docnum                 = e_docnum
     RECEIVING
       r_if_material_destinacao = r_if_material_destinacao ).

  ENDMETHOD.


  METHOD ZIF_MATERIAL_DESTINACAO~SET_VALIDAR.

    DATA: LC_LIFNR TYPE LIFNR,
          WA_LFA1  TYPE LFA1.

    R_IF_MATERIAL_DESTINACAO = SUPER->ZIF_MATERIAL_DESTINACAO~SET_VALIDAR( IMPORTING E_VALIDOU = E_VALIDOU ).

    CLEAR: E_VALIDOU.

    R_IF_MATERIAL_DESTINACAO = ME.

    "ZIF_MATERIAL_DESTINACAO=>ST_TP_DESTINACAO_ARMAZENAR

    "Somente Permitir Documentos de Material de Entrada
    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0115 INTO DATA(WA_ZMMT0115).

      SELECT * INTO TABLE @DATA(IT_MSEG)
        FROM MSEG
       WHERE MBLNR EQ @WA_ZMMT0115-ORIG_MBLNR
         AND MJAHR EQ @WA_ZMMT0115-ORIG_MJAHR
         AND XAUTO EQ @SPACE.

      LOOP AT IT_MSEG INTO DATA(WA_MSEG).
        IF WA_MSEG-SHKZG NE 'S'.
          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGID
                                MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGNO )
              MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGID
              MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ORIGEM_DOC_SAIDA_NAO-MSGNO
              MSGTY  = 'E'.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    "Verificar se Romaneio está Ativo
    LOOP AT ME->ZIF_MATERIAL_DESTINACAO~AT_ZMMT0118 INTO DATA(WA_MMT0118).

      TRY .

          CALL METHOD ZCL_ROMANEIO=>GET_STATUS_OPUS
            EXPORTING
              I_REFERENCIA   = WA_MMT0118-CH_REFERENCIA
              I_TP_MOVIMENTO = 'S'
            RECEIVING
              R_STATUS       = DATA(LC_STATUS).
        CATCH ZCX_ERROR INTO DATA(EX_ERROR).

          RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_ERROR->MSGID
                                MSGNO = EX_ERROR->MSGNO
                                ATTR1 = EX_ERROR->MSGV1
                                ATTR2 = EX_ERROR->MSGV2
                                ATTR3 = EX_ERROR->MSGV3
                                ATTR4 = EX_ERROR->MSGV4 )
              MSGID  = EX_ERROR->MSGID
              MSGNO  = EX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = EX_ERROR->MSGV1
              MSGV2  = EX_ERROR->MSGV2
              MSGV3  = EX_ERROR->MSGV3
              MSGV4  = EX_ERROR->MSGV4.

      ENDTRY.

      IF LC_STATUS NE ZIF_CARGA=>ST_STATUS_FECHADO.

        SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
          FROM ZSDT0001
         WHERE CH_REFERENCIA EQ @WA_MMT0118-CH_REFERENCIA.

        RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_NAO_FECHADO-MSGID
                              MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_NAO_FECHADO-MSGNO
                              ATTR1 = WA_ZSDT0001-NR_ROMANEIO
                              ATTR2 = WA_ZSDT0001-BRANCH
                              ATTR3 = WA_ZSDT0001-NR_SAFRA )
            MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_NAO_FECHADO-MSGID
            MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_NAO_FECHADO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( WA_ZSDT0001-NR_ROMANEIO )
            MSGV2  = CONV #( WA_ZSDT0001-BRANCH )
            MSGV3  = CONV #( WA_ZSDT0001-NR_SAFRA ).
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_EKPO)
        FROM EKPO
       WHERE EBELN EQ @WA_ZSDT0001-VBELN.

      SELECT SINGLE * INTO @DATA(WA_EKKO)
        FROM EKKO
       WHERE EBELN EQ @WA_ZSDT0001-VBELN.

      IF NOT ( WA_EKKO-BSTYP EQ 'F' AND WA_EKKO-BSART EQ 'ZARM' ).
        RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_PEDIDO_NAO_PERMITIDO-MSGID
                              MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_PEDIDO_NAO_PERMITIDO-MSGNO
                              ATTR1 = WA_EKKO-BSART )
            MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_PEDIDO_NAO_PERMITIDO-MSGID
            MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_PEDIDO_NAO_PERMITIDO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( WA_EKKO-BSART ).
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_EKPO-WERKS
        IMPORTING
          OUTPUT = LC_LIFNR.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          P_PARCEIRO   = LC_LIFNR
          P_PARTYPE    = 'B'
        CHANGING
          WA_INFO_PART = WA_LFA1.

      SELECT SINGLE T~DESCRIPT
        FROM SETLEAF AS S
       INNER JOIN SETLINET AS T ON T~SETNAME EQ S~SETNAME AND T~LINEID EQ S~LINEID
        INTO @DATA(VL_DESCRIPT)
       WHERE S~SETNAME = 'MAGGI_ZMM0019_IVA_SAIDA'
         AND S~VALFROM = @WA_LFA1-REGIO.

      IF SY-SUBRC IS NOT INITIAL OR VL_DESCRIPT IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_MATERIAL_DESTINACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_TAX_CODE-MSGID
                              MSGNO = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_TAX_CODE-MSGNO )
            MSGID  = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_TAX_CODE-MSGID
            MSGNO  = ZCX_MATERIAL_DESTINACAO=>ZCX_ROM_SAI_TAX_CODE-MSGNO
            MSGTY  = 'E'.
      ENDIF.

    ENDLOOP.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
