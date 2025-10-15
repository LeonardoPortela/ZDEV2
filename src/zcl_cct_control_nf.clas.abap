class ZCL_CCT_CONTROL_NF definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods ATRIBUIR_NF
    importing
      !I_ZLEST0142 type ZLEST0142 .
  methods ATRIBUIR_NF_COMPLEMENTADAS
    importing
      !I_NF_COMPLEMENTADAS type ZLEST0192_T .
  methods DISP_NF_CCT
    exporting
      !E_ZLEST0142 type ZLEST0142
    returning
      value(R_DISPONIBILIZADA) type CHAR01 .
  class-methods REMOVER_NF_CCT
    importing
      !I_ZLEST0142 type ZLEST0142
    returning
      value(R_REMOVIDA) type CHAR01 .
  methods ATRIBUIR_NF_ROM
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_NAO_VALIDAR type CHAR01 default ' '
    exporting
      !E_NOTA_FISCAL type ZLEST0142
    returning
      value(R_ATRIBUIDO) type CHAR01 .
  class-methods GET_ROMANEIOS_NF
    importing
      value(I_ZLEST0142) type ZLEST0142
    returning
      value(E_T_ZSDT0001) type ZSDT001 .
  class-methods ATUALIZA_ST_CCT_REGISTROS
    importing
      !I_ZLEST0142 type ZLEST0142
      !I_ST_CCT type ZST_CCT
    returning
      value(R_ATUALIZADO) type CHAR01 .
protected section.
private section.

  data AT_NF type ZLEST0142 .
  data AT_NF_COMPLEMENTADAS type ZLEST0192_T .

  methods VALIDA_ENVIO_NF_CCT
    returning
      value(R_VALIDADO) type CHAR01 .
  methods ATRIBUIR_NFE_ROM
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_NAO_VALIDAR type CHAR01 default ' '
    returning
      value(R_ATRIBUIDO) type CHAR01 .
  methods ATRIBUIR_NFF_ROM
    importing
      !I_CH_REFERENCIA type ZCH_REF
      !I_NAO_VALIDAR type CHAR01 default ' '
    returning
      value(R_ATRIBUIDO) type CHAR01 .
ENDCLASS.



CLASS ZCL_CCT_CONTROL_NF IMPLEMENTATION.


  method ATRIBUIR_NF.

    ME->AT_NF = I_ZLEST0142.

  endmethod.


  METHOD atribuir_nfe_rom.

    DATA: it_zsdt0001 TYPE TABLE OF zsdt0001,
          wl_zsdt0001 TYPE zsdt0001.

    DATA: r_loc_descarga  TYPE RANGE OF zsdt0001-local_descarga,
          wl_loc_descarga LIKE LINE OF r_loc_descarga.

    r_atribuido = abap_false.

    CLEAR: me->at_nf.

    IF i_nao_validar EQ abap_false.

      "Get Local Descargas parametrizados.
      SELECT *
        FROM setleaf INTO TABLE @DATA(_tg_set_leaf)
       WHERE setname = 'ZLES0147_ROM_CCT'.

      DELETE _tg_set_leaf WHERE valfrom IS INITIAL.

      CHECK _tg_set_leaf[] IS NOT INITIAL.

      LOOP AT _tg_set_leaf INTO DATA(wl_set).
        wl_loc_descarga-sign   = 'I'.
        wl_loc_descarga-option = 'EQ'.
        wl_loc_descarga-low    = wl_set-valfrom.
        APPEND wl_loc_descarga TO r_loc_descarga.
      ENDLOOP.

    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(_wl_0001)
     WHERE ch_referencia EQ @i_ch_referencia.

    CHECK ( sy-subrc = 0 ) AND ( strlen( _wl_0001-chave_nfe ) EQ 44 ).

    CLEAR: it_zsdt0001[].

    SELECT *
      FROM zsdt0001 AS a INTO TABLE it_zsdt0001
     WHERE a~chave_nfe       EQ _wl_0001-chave_nfe
       AND a~tp_movimento    EQ 'E'
       AND a~st_cct          IN ( '', '01', '03' )
       AND a~local_descarga  IN r_loc_descarga
       AND EXISTS ( SELECT *
                      FROM zlest0149 AS b
                     WHERE b~bukrs      =  a~bukrs
                       AND b~branch     =  a~branch
                       AND b~dt_ini_rom <= a~dt_fechamento ).

    CHECK it_zsdt0001[] IS NOT INITIAL.

    "Recupera ultimo romaneio para a NF-e.
    SORT it_zsdt0001 BY dt_fechamento DESCENDING.
    LOOP AT it_zsdt0001 INTO wl_zsdt0001.
      DATA(_wl_max_0001) = wl_zsdt0001.
      EXIT.
    ENDLOOP.

    me->at_nf-chave_nfe           =  _wl_max_0001-chave_nfe.
    me->at_nf-cnpj_emissor        =  _wl_max_0001-chave_nfe+06(14).
    me->at_nf-numero              =  _wl_max_0001-chave_nfe+25(9).
    me->at_nf-dt_emissao          =  _wl_max_0001-docdat.
    me->at_nf-serie               =  _wl_max_0001-chave_nfe+22(3).
    me->at_nf-model               =  _wl_max_0001-chave_nfe+20(2).
    me->at_nf-dt_chegada          =  _wl_max_0001-dt_fechamento.
    me->at_nf-bukrs_rom           =  _wl_max_0001-bukrs.
    me->at_nf-branch_rom          =  _wl_max_0001-branch.
    me->at_nf-bukrs_ra            =  _wl_max_0001-bukrs.
    me->at_nf-branch_ra           =  _wl_max_0001-branch.
    me->at_nf-matnr               =  _wl_max_0001-matnr.
    me->at_nf-local_descarga      =  _wl_max_0001-local_descarga.
    me->at_nf-chave_cte           =  _wl_max_0001-chave_cte.
    me->at_nf-modal               =  _wl_max_0001-modal.
    me->at_nf-peso_fiscal         =  _wl_max_0001-peso_fiscal.

*** CS2019001041 - Inicio - CBRAND
    IF me->at_nf-modal = '04'.

      CLEAR: me->at_nf-peso_fiscal.
      me->at_nf-peso_transbordo  = _wl_max_0001-peso_fiscal.

      SELECT SINGLE *
        FROM zib_nfe_dist_itm  INTO @DATA(_wl_zib_nfe_dist_itm)
       WHERE chave_nfe  EQ @_wl_max_0001-chave_nfe.

      IF  sy-subrc = 0.

        _wl_zib_nfe_dist_itm-prod_und_comerci = |{ _wl_zib_nfe_dist_itm-prod_und_comerci CASE = UPPER }|.

        IF _wl_zib_nfe_dist_itm-prod_und_comerci = 'KG'.
          me->at_nf-peso_fiscal = _wl_zib_nfe_dist_itm-prod_qtd_comerci.
        ELSE.
          IF _wl_zib_nfe_dist_itm-prod_und_comerci = 'TO'
            OR _wl_zib_nfe_dist_itm-prod_und_comerci = 'TON'.

            me->at_nf-peso_fiscal = _wl_zib_nfe_dist_itm-prod_qtd_comerci / 1000.
          ELSE.
            IF _wl_zib_nfe_dist_itm-prod_und_comerci NE 'TO'
              AND _wl_zib_nfe_dist_itm-prod_und_comerci NE 'TON'
              AND _wl_zib_nfe_dist_itm-prod_und_comerci NE 'KG'.

              me->at_nf-peso_fiscal = 0.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.

        SELECT *
          FROM j_1bnfe_active INTO TABLE @DATA(_tg_j_1bnfe_active)
         WHERE regio    = @_wl_max_0001-chave_nfe+00(02)
          AND  nfyear   = @_wl_max_0001-chave_nfe+02(02)
          AND  nfmonth  = @_wl_max_0001-chave_nfe+04(02)
          AND  stcd1    = @_wl_max_0001-chave_nfe+06(14)
          AND  model    = @_wl_max_0001-chave_nfe+20(02)
          AND  serie    = @_wl_max_0001-chave_nfe+22(03)
          AND  nfnum9   = @_wl_max_0001-chave_nfe+25(09)
          AND  docnum9  = @_wl_max_0001-chave_nfe+34(09)
          AND  cdv      = @_wl_max_0001-chave_nfe+43(01)
          AND  direct   = '2'
          AND  form    <> ' '.

        IF sy-subrc = 0.

          SELECT *
            FROM j_1bnfdoc INTO TABLE @DATA(_tg_j_1bnfdoc)
             FOR ALL ENTRIES IN @_tg_j_1bnfe_active
          WHERE docnum  = @_tg_j_1bnfe_active-docnum
             AND candat = '00000000'
             AND cancel =  ' '.

          IF sy-subrc = 0.
            SELECT *
              FROM j_1bnflin INTO TABLE @DATA(_tg_j_1bnflin)
               FOR ALL ENTRIES IN @_tg_j_1bnfdoc
            WHERE docnum = @_tg_j_1bnfdoc-docnum.

            IF sy-subrc = 0.
              READ TABLE _tg_j_1bnflin INTO DATA(_wl_j_1bnflin) INDEX 1.
              me->at_nf-peso_fiscal = _wl_j_1bnflin-menge.
            ELSE.
              me->at_nf-peso_fiscal = 0.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF me->at_nf-peso_fiscal <= 0.
        EXIT.
      ENDIF.
    ENDIF.
*** CS2019001041 - Fim - CBRAND

    LOOP AT it_zsdt0001 INTO wl_zsdt0001.
      ADD wl_zsdt0001-peso_subtotal TO me->at_nf-peso_chegada.

      IF me->at_nf-modal = '04'.
        ADD wl_zsdt0001-peso_rateio_origem TO me->at_nf-peso_rateio_origem.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = _wl_max_0001-agente_frete
      IMPORTING
        output = _wl_max_0001-agente_frete.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1)
     WHERE lifnr = @_wl_max_0001-agente_frete.

    IF ( sy-subrc = 0 ) AND ( _wl_max_0001-agente_frete IS NOT INITIAL ) AND ( _wl_lfa1-stcd1 IS NOT INITIAL OR _wl_lfa1-stcd2 IS NOT INITIAL ) AND ( me->at_nf-modal NE '04' ).
      me->at_nf-cnpj_transp       = _wl_lfa1-stcd1.
      me->at_nf-cpf_transp        = _wl_lfa1-stcd2.
    ENDIF.

    IF ( me->at_nf-cnpj_transp IS INITIAL AND me->at_nf-cpf_transp IS INITIAL ) AND ( me->at_nf-modal NE '04' ).
      SELECT SINGLE *
        FROM zsdt0001 INTO @DATA(wl_0001)
       WHERE chave_nfe = @_wl_max_0001-chave_nfe.

      IF ( sy-subrc = 0 ) AND ( wl_0001-doc_rem IS NOT INITIAL ).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_0001-doc_rem
          IMPORTING
            output = wl_0001-doc_rem.

        SELECT SINGLE *
          FROM vbpa INTO @DATA(wl_vbpa)
         WHERE vbeln = @wl_0001-doc_rem
           AND parvw = 'SP'.

        IF ( sy-subrc = 0 ) AND ( wl_vbpa-lifnr IS NOT INITIAL ).

          SELECT SINGLE *
            FROM lfa1 INTO _wl_lfa1
           WHERE lifnr = wl_vbpa-lifnr.
          IF ( sy-subrc = 0 ) AND ( _wl_lfa1-stcd1 IS NOT INITIAL  OR _wl_lfa1-stcd2 IS NOT INITIAL ).
            me->at_nf-cnpj_transp       = _wl_lfa1-stcd1.
            me->at_nf-cpf_transp        = _wl_lfa1-stcd2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( me->at_nf-cnpj_transp IS INITIAL     ) AND
       ( me->at_nf-cpf_transp  IS INITIAL     ) AND
       ( _wl_0001-chave_nfe    IS NOT INITIAL ) AND
       ( me->at_nf-modal       NE '04'        ).

      SELECT SINGLE *
        FROM zib_nfe_dist_ter INTO @DATA(_wl_zib_nfe_dist_ter)
       WHERE chave_nfe = @_wl_0001-chave_nfe.

      IF ( sy-subrc = 0 ).
        IF _wl_zib_nfe_dist_ter-transportador_cnpj IS NOT INITIAL.
          me->at_nf-cnpj_transp = _wl_zib_nfe_dist_ter-transportador_cnpj.
        ELSEIF _wl_zib_nfe_dist_ter-transportador_cpf IS NOT INITIAL.
          me->at_nf-cpf_transp = _wl_zib_nfe_dist_ter-transportador_cpf.
        ENDIF.
      ENDIF.

    ENDIF.

    r_atribuido = abap_true.


  ENDMETHOD.


  method ATRIBUIR_NFF_ROM.

    DATA: WL_ZSDT0001     TYPE ZSDT0001,
          WL_RETORNO_PROC TYPE ZDE_RETORNO_PROC,
          WL_DOC_TMP      TYPE J_1BNFDOC.

    DATA: R_LOC_DESCARGA  TYPE RANGE OF ZSDT0001-LOCAL_DESCARGA,
          WL_LOC_DESCARGA LIKE LINE OF R_LOC_DESCARGA.

    R_ATRIBUIDO = ABAP_FALSE.

    CLEAR: ME->AT_NF.

    IF I_NAO_VALIDAR EQ ABAP_FALSE.

      "Get Local Descargas parametrizados.
      SELECT *
        FROM SETLEAF INTO TABLE @DATA(_TG_SET_LEAF)
       WHERE SETNAME = 'ZLES0147_ROM_CCT'.

      DELETE _TG_SET_LEAF WHERE VALFROM IS INITIAL.

      CHECK _TG_SET_LEAF[] IS NOT INITIAL.

      LOOP AT _TG_SET_LEAF INTO DATA(WL_SET).
        WL_LOC_DESCARGA-SIGN   = 'I'.
        WL_LOC_DESCARGA-OPTION = 'EQ'.
        WL_LOC_DESCARGA-LOW    = WL_SET-VALFROM.
        APPEND WL_LOC_DESCARGA TO R_LOC_DESCARGA.
      ENDLOOP.

    ENDIF.

    CLEAR: WL_ZSDT0001.

    SELECT SINGLE *
      FROM ZSDT0001 AS A INTO WL_ZSDT0001
     WHERE A~CH_REFERENCIA   EQ I_CH_REFERENCIA
       AND A~CHAVE_NFE       EQ SPACE
       AND A~TP_MOVIMENTO    EQ 'E'
       AND A~ST_CCT          IN ( '', '01', '03' )
       AND A~LOCAL_DESCARGA  IN R_LOC_DESCARGA
       AND EXISTS ( SELECT *
                      FROM ZLEST0149 AS B
                     WHERE B~BUKRS      =  A~BUKRS
                       AND B~BRANCH     =  A~BRANCH
                       AND B~DT_INI_ROM <= A~DT_FECHAMENTO ).

    CHECK ( SY-SUBRC EQ 0 ) AND ( WL_ZSDT0001 IS NOT INITIAL ).

    SELECT SINGLE *
      FROM LFA1 INTO @DATA(_WL_LFA1)
     WHERE LIFNR = @WL_ZSDT0001-PARID.

    CHECK ( SY-SUBRC EQ 0 ) AND ( _WL_LFA1-STKZN IS NOT INITIAL ).

    SELECT SINGLE *
      FROM ADRC INTO @DATA(_WL_ADRC)
     WHERE ADDRNUMBER = @_WL_LFA1-ADRNR.

    CHECK SY-SUBRC EQ 0.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = WL_ZSDT0001-SERIES
      IMPORTING
        OUTPUT        = WL_ZSDT0001-SERIES.

    CHECK NOT ( ( WL_ZSDT0001-SERIES GE '890' ) AND ( WL_ZSDT0001-SERIES LE '899' ) ).
    CHECK NOT ( ( WL_ZSDT0001-SERIES GE '900' ) AND ( WL_ZSDT0001-SERIES LE '999' ) ).

    "Definir Chave NF-f
    CLEAR: WL_DOC_TMP.
    WL_DOC_TMP-NFNUM   = WL_ZSDT0001-NFNUM.
    WL_DOC_TMP-MODEL   = '04'.
    WL_DOC_TMP-SERIES  = WL_ZSDT0001-SERIES.
    WL_DOC_TMP-DOCDAT  = WL_ZSDT0001-DOCDAT.
    WL_DOC_TMP-PARID   = WL_ZSDT0001-PARID.
    WL_DOC_TMP-PARTYP  = 'V'.

    CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
      EXPORTING
        I_J_1BNFDOC       = WL_DOC_TMP
      IMPORTING
        E_CHAVE_NFF       = ME->AT_NF-CHAVE_NFF
        E_RETORNO         = WL_RETORNO_PROC.

    IF WL_RETORNO_PROC-TYPE EQ 'E'.
      RETURN.
    ENDIF.

    ME->AT_NF-CPF_EMISSOR         =  _WL_LFA1-STCD2.
    ME->AT_NF-NUMERO              =  WL_ZSDT0001-NFNUM.
    ME->AT_NF-DT_EMISSAO          =  WL_ZSDT0001-DOCDAT.
    ME->AT_NF-SERIE               =  WL_ZSDT0001-SERIES.
    ME->AT_NF-MODEL               =  '04'.
    ME->AT_NF-DT_CHEGADA          =  WL_ZSDT0001-DT_FECHAMENTO.
    ME->AT_NF-BUKRS_ROM           =  WL_ZSDT0001-BUKRS.
    ME->AT_NF-BRANCH_ROM          =  WL_ZSDT0001-BRANCH.
    ME->AT_NF-BUKRS_RA            =  WL_ZSDT0001-BUKRS.
    ME->AT_NF-BRANCH_RA           =  WL_ZSDT0001-BRANCH.
    ME->AT_NF-MATNR               =  WL_ZSDT0001-MATNR.
    ME->AT_NF-LOCAL_DESCARGA      =  WL_ZSDT0001-LOCAL_DESCARGA.
    ME->AT_NF-CHAVE_CTE           =  WL_ZSDT0001-CHAVE_CTE.
    ME->AT_NF-MODAL               =  WL_ZSDT0001-MODAL.
    ME->AT_NF-PESO_FISCAL         =  WL_ZSDT0001-PESO_FISCAL.
    ME->AT_NF-NETWR               =  WL_ZSDT0001-NETWR.
    ME->AT_NF-CFOP                =  WL_ZSDT0001-CFOP.
    ME->AT_NF-PESO_CHEGADA        =  WL_ZSDT0001-PESO_SUBTOTAL.
    ME->AT_NF-CH_REFERENCIA       =  WL_ZSDT0001-CH_REFERENCIA.
    ME->AT_NF-SIGLA_UF_EMISSOR    =  'BR' && '-' && _WL_ADRC-REGION.

    IF ( WL_ZSDT0001-AGENTE_FRETE IS NOT INITIAL ).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT      = WL_ZSDT0001-AGENTE_FRETE
        IMPORTING
          OUTPUT     = WL_ZSDT0001-AGENTE_FRETE.

      SELECT SINGLE *
        FROM LFA1 INTO _WL_LFA1
       WHERE LIFNR = WL_ZSDT0001-AGENTE_FRETE.

      IF ( SY-SUBRC = 0 ) AND ( _WL_LFA1-STCD1 IS NOT INITIAL OR _WL_LFA1-STCD2 IS NOT INITIAL ) AND ( ME->AT_NF-MODAL NE '04' ).
        ME->AT_NF-CNPJ_TRANSP       = _WL_LFA1-STCD1.
        ME->AT_NF-CPF_TRANSP        = _WL_LFA1-STCD2.
      ENDIF.
    ENDIF.

    R_ATRIBUIDO = ABAP_TRUE.

  endmethod.


  method ATRIBUIR_NF_COMPLEMENTADAS.

    ME->AT_NF_COMPLEMENTADAS = I_NF_COMPLEMENTADAS.

  endmethod.


  method ATRIBUIR_NF_ROM.

    CLEAR: E_NOTA_FISCAL.

    R_ATRIBUIDO = ABAP_FALSE.

    CHECK I_CH_REFERENCIA IS NOT INITIAL.

    SELECT SINGLE *
      FROM ZSDT0001 INTO @DATA(_WL_0001)
     WHERE CH_REFERENCIA EQ @I_CH_REFERENCIA.

    CHECK ( SY-SUBRC = 0 ).

    IF _WL_0001-CHAVE_NFE IS NOT INITIAL.
      R_ATRIBUIDO = ME->ATRIBUIR_NFE_ROM( I_CH_REFERENCIA = I_CH_REFERENCIA I_NAO_VALIDAR = I_NAO_VALIDAR ).
    ELSE.
      R_ATRIBUIDO = ME->ATRIBUIR_NFF_ROM( I_CH_REFERENCIA = I_CH_REFERENCIA I_NAO_VALIDAR = I_NAO_VALIDAR ).
    ENDIF.

    IF R_ATRIBUIDO EQ ABAP_TRUE.
      E_NOTA_FISCAL = ME->AT_NF.
    ENDIF.

  endmethod.


  method ATUALIZA_ST_CCT_REGISTROS.

    R_ATUALIZADO = ABAP_FALSE.

    CHECK ( I_ZLEST0142 IS NOT INITIAL ).

    "Vinculos Aquáviario
    IF I_ZLEST0142-MODAL EQ '03'.

      IF I_ZLEST0142-CHAVE_NFE IS NOT INITIAL.
        UPDATE ZLEST0060 SET ST_CCT = I_ST_CCT
         WHERE CHAVE_NFE = I_ZLEST0142-CHAVE_NFE.
      ENDIF.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE S124.
        RETURN.
      ENDIF.

    ENDIF.

    "Vinculos ZNFW
    IF I_ZLEST0142-SEQ_LCTO IS NOT INITIAL.
      UPDATE ZFIWRT0008 SET ST_CCT = I_ST_CCT
       WHERE SEQ_LCTO EQ I_ZLEST0142-SEQ_LCTO.

       IF SY-SUBRC NE 0.
         ROLLBACK WORK.
         MESSAGE S124.
         RETURN.
       ENDIF.
    ENDIF.

    "Vinculos Romaneio
    DATA(_TG_0001) = ZCL_CCT_CONTROL_NF=>GET_ROMANEIOS_NF( I_ZLEST0142 = I_ZLEST0142 ).

    LOOP AT _TG_0001 INTO DATA(_WL_0001).
      UPDATE ZSDT0001 SET ST_CCT = I_ST_CCT
       WHERE CH_REFERENCIA = _WL_0001-CH_REFERENCIA.
    ENDLOOP.

    R_ATUALIZADO = ABAP_TRUE.

  endmethod.


  method CONSTRUCTOR.

    CLEAR: ME->AT_NF,
           ME->AT_NF_COMPLEMENTADAS.

  endmethod.


  method DISP_NF_CCT.

    DATA: IT_ACTIVE   TYPE TABLE OF J_1BNFE_ACTIVE,
          V_CANDAT    TYPE J_1BNFDOC-CANDAT.

    CLEAR: E_ZLEST0142.

    R_DISPONIBILIZADA = ABAP_FALSE.

*-------------------------------------------------------------------------------------------*
*    Completa Dados
*-------------------------------------------------------------------------------------------*

    IF ( ME->AT_NF-BUKRS_ROM IS NOT INITIAL ) AND ( ME->AT_NF-BRANCH_ROM IS NOT INITIAL ).
      SELECT SINGLE *
        FROM ZLEST0149 INTO @DATA(WL_ZLEST0149)
       WHERE BUKRS   EQ @ME->AT_NF-BUKRS_ROM
         AND BRANCH  EQ @ME->AT_NF-BRANCH_ROM.

      IF ( SY-SUBRC EQ 0 ) AND ( WL_ZLEST0149-BUKRS_RA IS NOT INITIAL ) AND ( WL_ZLEST0149-BRANCH_RA IS NOT INITIAL ).
        ME->AT_NF-BUKRS_RA   =  WL_ZLEST0149-BUKRS_RA.
        ME->AT_NF-BRANCH_RA  =  WL_ZLEST0149-BRANCH_RA.
      ENDIF.
    ENDIF.

    IF ( ME->AT_NF-DOCNUM IS INITIAL ) AND ( ME->AT_NF-CHAVE_NFE IS NOT INITIAL ).
      CLEAR: IT_ACTIVE[], V_CANDAT.

      SELECT A~* INTO CORRESPONDING FIELDS OF TABLE @IT_ACTIVE
        FROM J_1BNFE_ACTIVE AS A INNER JOIN J_1BNFDOC AS B ON A~DOCNUM = B~DOCNUM
       WHERE A~REGIO    EQ @ME->AT_NF-CHAVE_NFE+00(02)
         AND A~NFYEAR   EQ @ME->AT_NF-CHAVE_NFE+02(02)
         AND A~NFMONTH  EQ @ME->AT_NF-CHAVE_NFE+04(02)
         AND A~STCD1    EQ @ME->AT_NF-CHAVE_NFE+06(14)
         AND A~MODEL    EQ @ME->AT_NF-CHAVE_NFE+20(02)
         AND A~SERIE    EQ @ME->AT_NF-CHAVE_NFE+22(03)
         AND A~NFNUM9   EQ @ME->AT_NF-CHAVE_NFE+25(09)
         AND A~DOCNUM9  EQ @ME->AT_NF-CHAVE_NFE+34(09)
         AND A~CDV      EQ @ME->AT_NF-CHAVE_NFE+43(01)
         AND A~DIRECT   EQ '2'
         AND A~FORM     NE ' '
         AND B~CANDAT   EQ @V_CANDAT
         AND B~CANCEL   EQ @SPACE.

      IF IT_ACTIVE[] IS NOT INITIAL.
        READ TABLE IT_ACTIVE INTO DATA(_WL_ACTIVE) INDEX 1.
        ME->AT_NF-DOCNUM = _WL_ACTIVE-DOCNUM.
      ENDIF.
    ENDIF.

    IF ( ME->AT_NF-DOCNUM IS NOT INITIAL ) AND ( ME->AT_NF-CFOP IS INITIAL ).
      SELECT SINGLE *
        FROM J_1BNFLIN INTO @DATA(WL_LIN)
       WHERE DOCNUM EQ @ME->AT_NF-DOCNUM.

      IF SY-SUBRC EQ 0.
        ME->AT_NF-CFOP = WL_LIN-CFOP(4).
      ENDIF.
    ENDIF.

*-------------------------------------------------------------------------------------------*
*   Validar Dados
*-------------------------------------------------------------------------------------------*

    DATA(_VALIDADO) = ME->VALIDA_ENVIO_NF_CCT( ).

    CHECK _VALIDADO EQ ABAP_TRUE.

*-------------------------------------------------------------------------------------------*
*   Gravar Registros
*-------------------------------------------------------------------------------------------*
    ME->AT_NF-DATA_REG   =  SY-DATUM.
    ME->AT_NF-HORA_REG   =  SY-UZEIT.
    ME->AT_NF-USNAM_REG  =  SY-UNAME.

    MODIFY ZLEST0142 FROM ME->AT_NF.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE S141(ZCCT).
      RETURN.
    ENDIF.

    IF ME->AT_NF_COMPLEMENTADAS[] IS NOT INITIAL.

      "Remover registros anteriores...
      IF ME->AT_NF-CHAVE_NFE IS NOT INITIAL.
        DELETE FROM ZLEST0192 WHERE CHAVE = ME->AT_NF-CHAVE_NFE.
      ELSEIF ME->AT_NF-CHAVE_NFF IS NOT INITIAL.
        DELETE FROM ZLEST0192 WHERE CHAVE = ME->AT_NF-CHAVE_NFF.
      ENDIF.

      MODIFY ZLEST0192 FROM TABLE ME->AT_NF_COMPLEMENTADAS.
      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE S142(ZCCT).
        RETURN.
      ENDIF.
    ENDIF.

    "Gerar Vinculos CCT
    DATA(_ATUALIZADO) = ZCL_CCT_CONTROL_NF=>ATUALIZA_ST_CCT_REGISTROS( EXPORTING I_ZLEST0142  =   ME->AT_NF
                                                                                 I_ST_CCT     =  '01'  "Disponibilizado Registro CCT
                                                                      ).
    IF _ATUALIZADO EQ ABAP_FALSE.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK.

    E_ZLEST0142 = ME->AT_NF.

    R_DISPONIBILIZADA = ABAP_TRUE.

    MESSAGE S117(ZCCT).


  endmethod.


  method GET_ROMANEIOS_NF.

    DATA: WL_ZSDS001 TYPE ZSDS001,
          V_VBELN    TYPE ZDOC_REM,
          TG_0001    TYPE TABLE OF ZSDT0001,
          IT_ACTIVE  TYPE TABLE OF J_1BNFE_ACTIVE,
          V_CANDAT   TYPE J_1BNFDOC-CANDAT.

    CLEAR: E_T_ZSDT0001[], TG_0001[].

    IF I_ZLEST0142-CHAVE_NFE IS NOT INITIAL.

      IF I_ZLEST0142-MODAL = '03'. "Aquaviario
        SELECT *
          FROM ZLEST0060 INTO TABLE @DATA(_TG_0060)
         WHERE CHAVE_NFE = @I_ZLEST0142-CHAVE_NFE.

        DELETE _TG_0060[] WHERE CH_REFERENCIA IS INITIAL.

        IF _TG_0060[] IS NOT INITIAL.
          SELECT *
            FROM ZSDT0001 INTO TABLE TG_0001
             FOR ALL ENTRIES IN _TG_0060
           WHERE CH_REFERENCIA = _TG_0060-CH_REFERENCIA.
        ENDIF.
      ELSE.
        SELECT *
          FROM ZSDT0001 INTO TABLE TG_0001
         WHERE CHAVE_NFE = I_ZLEST0142-CHAVE_NFE.
      ENDIF.

      IF TG_0001[] IS INITIAL.

         CLEAR: IT_ACTIVE[].

         SELECT A~* INTO CORRESPONDING FIELDS OF TABLE @IT_ACTIVE
          FROM J_1BNFE_ACTIVE AS A INNER JOIN J_1BNFDOC AS B ON A~DOCNUM = B~DOCNUM
         WHERE A~REGIO    EQ @I_ZLEST0142-CHAVE_NFE+00(02)
           AND A~NFYEAR   EQ @I_ZLEST0142-CHAVE_NFE+02(02)
           AND A~NFMONTH  EQ @I_ZLEST0142-CHAVE_NFE+04(02)
           AND A~STCD1    EQ @I_ZLEST0142-CHAVE_NFE+06(14)
           AND A~MODEL    EQ @I_ZLEST0142-CHAVE_NFE+20(02)
           AND A~SERIE    EQ @I_ZLEST0142-CHAVE_NFE+22(03)
           AND A~NFNUM9   EQ @I_ZLEST0142-CHAVE_NFE+25(09)
           AND A~DOCNUM9  EQ @I_ZLEST0142-CHAVE_NFE+34(09)
           AND A~CDV      EQ @I_ZLEST0142-CHAVE_NFE+43(01)
           AND A~DIRECT   EQ '2'
           AND B~DOCDAT   EQ @I_ZLEST0142-DT_EMISSAO
           AND A~FORM     NE ' '
           AND B~CANDAT   EQ @V_CANDAT
           AND B~CANCEL   EQ @SPACE.

        DELETE IT_ACTIVE WHERE DOCNUM IS INITIAL.
        READ TABLE IT_ACTIVE INTO DATA(_WL_ACTIVE) INDEX 1.

        IF _WL_ACTIVE-DOCNUM IS NOT INITIAL.

          CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
            EXPORTING
              I_DOCNUM       = _WL_ACTIVE-DOCNUM
              I_DIRECT       = '2'
            IMPORTING
              E_VBELN        = V_VBELN.

          IF V_VBELN IS NOT INITIAL.
            SELECT *
              FROM ZSDT0001 INTO TABLE TG_0001
             WHERE DOC_REM       EQ V_VBELN
               AND TP_MOVIMENTO  EQ 'S'.
          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF I_ZLEST0142-CHAVE_NFF IS NOT INITIAL.

      SELECT *
        FROM ZSDT0001 INTO TABLE TG_0001
       WHERE CH_REFERENCIA = I_ZLEST0142-CH_REFERENCIA.

    ENDIF.

    LOOP AT TG_0001 INTO DATA(_WL_0001).
      CLEAR: WL_ZSDS001.
      MOVE-CORRESPONDING _WL_0001 TO WL_ZSDS001.

      APPEND WL_ZSDS001 TO E_T_ZSDT0001.
    ENDLOOP.

  endmethod.


  method REMOVER_NF_CCT.

    DATA: V_CHAVE TYPE C LENGTH 44.

    R_REMOVIDA = ABAP_FALSE.

    CHECK ( I_ZLEST0142-CHAVE_NFE IS NOT INITIAL ) OR ( I_ZLEST0142-CHAVE_NFF IS NOT INITIAL ).

    IF I_ZLEST0142-CHAVE_NFE IS NOT INITIAL.
      V_CHAVE = I_ZLEST0142-CHAVE_NFE.
    ELSEIF I_ZLEST0142-CHAVE_NFF IS NOT INITIAL.
      V_CHAVE = I_ZLEST0142-CHAVE_NFF.
    ENDIF.

    SELECT SINGLE *
      FROM ZLEST0142 INTO @DATA(_WL_0142)
     WHERE CHAVE_NFE EQ @I_ZLEST0142-CHAVE_NFE
       AND CHAVE_NFF EQ @I_ZLEST0142-CHAVE_NFF.

    IF SY-SUBRC NE 0.
      MESSAGE |NF { V_CHAVE } não encontrada para processamento!| TYPE 'S'.
      RETURN.
    ENDIF.

    IF _WL_0142-ID_RECEPCAO IS NOT INITIAL.
      MESSAGE |Já existe um recepção de carga para a NF {  V_CHAVE }!| TYPE 'S'.
      RETURN.
    ENDIF.

    "Remover Vinculos CCT
    DATA(_ATUALIZADO) = ZCL_CCT_CONTROL_NF=>ATUALIZA_ST_CCT_REGISTROS( EXPORTING I_ZLEST0142  =  _WL_0142
                                                                                 I_ST_CCT     =  SPACE  ).
    IF _ATUALIZADO EQ ABAP_FALSE.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

*-------------------------------------------------------------------------------------------*
*   Remover Registro NF
*-------------------------------------------------------------------------------------------*

    DELETE FROM ZLEST0142 WHERE CHAVE_NFE = _WL_0142-CHAVE_NFE
                            AND CHAVE_NFF = _WL_0142-CHAVE_NFF.
    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE |Houve um erro ao remover a NF { V_CHAVE }!| TYPE 'S'.
      RETURN.
    ENDIF.

    IF _WL_0142-COMPLEMENTO EQ ABAP_TRUE.
      "Remover Notas Fiscais complementadas

      IF _WL_0142-CHAVE_NFE IS NOT INITIAL.

        SELECT SINGLE *
          FROM ZLEST0192 INTO @DATA(_WL_0192)
         WHERE CHAVE EQ @_WL_0142-CHAVE_NFE.

        IF SY-SUBRC EQ 0.
          DELETE FROM ZLEST0192 WHERE CHAVE = _WL_0142-CHAVE_NFE.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            MESSAGE S143 WITH _WL_0142-CHAVE_NFE.
            RETURN.
          ENDIF.
        ENDIF.

      ELSEIF _WL_0142-CHAVE_NFF IS NOT INITIAL.

        SELECT SINGLE *
          FROM ZLEST0192 INTO _WL_0192
         WHERE CHAVE EQ _WL_0142-CHAVE_NFF.

        IF SY-SUBRC EQ 0.
          DELETE FROM ZLEST0192 WHERE CHAVE = _WL_0142-CHAVE_NFF.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            MESSAGE S143 WITH _WL_0142-CHAVE_NFF.
            RETURN.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    MESSAGE S146(ZCCT).

    R_REMOVIDA = ABAP_TRUE.

  endmethod.


  method VALIDA_ENVIO_NF_CCT.

    DATA: WL_ZLEST0147 TYPE ZLEST0147.

    R_VALIDADO = ABAP_FALSE.

    SELECT *
      FROM SETLEAF INTO TABLE @DATA(GT_CFOPS_RIN)
     WHERE SETNAME EQ 'MAGGI_CFOP_RIN'.


    IF ( ME->AT_NF-CHAVE_NFE IS INITIAL ) AND ( ME->AT_NF-CHAVE_NFF IS INITIAL ).
      MESSAGE S118.
      RETURN.
    ENDIF.

    IF ( ME->AT_NF-CHAVE_NFE IS NOT INITIAL ) AND ( ME->AT_NF-CHAVE_NFF IS NOT INITIAL ).
      MESSAGE S118.
      RETURN.
    ENDIF.

    IF ( ME->AT_NF-MODEL IS INITIAL ).
      MESSAGE S085.
      RETURN.
    ENDIF.

    IF ( ME->AT_NF-CHAVE_NFE IS NOT INITIAL ).

      "Verifica se chave nf-e já foi enviada para o CCT
      SELECT SINGLE *
        FROM ZLEST0142 INTO @DATA(_WL_0142)
       WHERE CHAVE_NFE = @ME->AT_NF-CHAVE_NFE.

      IF ( SY-SUBRC EQ 0 ) AND ( _WL_0142-ID_RECEPCAO IS NOT INITIAL ).
        MESSAGE S119.
        RETURN.
      ENDIF.

    ELSEIF ( ME->AT_NF-CHAVE_NFF IS NOT INITIAL ).

      "Verifica se chave Nf-f já foi enviada para o CCT
      SELECT SINGLE *
        FROM ZLEST0142 INTO _WL_0142
       WHERE CHAVE_NFF = ME->AT_NF-CHAVE_NFF.

      IF ( SY-SUBRC EQ 0 ) AND ( _WL_0142-ID_RECEPCAO IS NOT INITIAL ).
        MESSAGE S119.
        RETURN.
      ENDIF.

    ENDIF.

    CASE ME->AT_NF-COMPLEMENTO.
      WHEN ABAP_TRUE.

        IF ME->AT_NF_COMPLEMENTADAS[] IS INITIAL.
          MESSAGE S139.
          RETURN.
        ENDIF.

        LOOP AT ME->AT_NF_COMPLEMENTADAS INTO DATA(_WL_NF_COMPLEMENTADA).

          IF _WL_NF_COMPLEMENTADA-CHAVE_COMP IS INITIAL.
            MESSAGE S148.
            RETURN.
          ENDIF.

          "Checar se NF complementada possui Recepcao de Carga no CCT
          IF _WL_NF_COMPLEMENTADA-CHAVE_COMP(1) = 'F'. "Nota Formulario

            SELECT SINGLE * INTO WL_ZLEST0147
              FROM ZLEST0147 AS A
             WHERE A~CHAVE_NFF EQ _WL_NF_COMPLEMENTADA-CHAVE_COMP
               AND EXISTS ( SELECT ID_RECEPCAO
                              FROM ZLEST0146 AS B
                             WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                               AND B~CANCEL      EQ ABAP_FALSE ).
          ELSE.

            SELECT SINGLE * INTO WL_ZLEST0147
              FROM ZLEST0147 AS A
             WHERE A~CHAVE_NFE EQ _WL_NF_COMPLEMENTADA-CHAVE_COMP
               AND EXISTS ( SELECT ID_RECEPCAO
                              FROM ZLEST0146 AS B
                             WHERE B~ID_RECEPCAO EQ A~ID_RECEPCAO
                               AND B~CANCEL      EQ ABAP_FALSE ).


          ENDIF.

          IF SY-SUBRC NE 0.
            MESSAGE S147 WITH _WL_NF_COMPLEMENTADA-CHAVE_COMP.
            RETURN.
          ENDIF.

        ENDLOOP.


      WHEN ABAP_FALSE.
        IF ME->AT_NF_COMPLEMENTADAS[] IS NOT INITIAL.
          MESSAGE S140.
          RETURN.
        ENDIF.

        IF ME->AT_NF-CFOP IS NOT INITIAL.
          READ TABLE GT_CFOPS_RIN INTO DATA(WL_CFOP_RIN) WITH KEY VALFROM = ME->AT_NF-CFOP.
          IF SY-SUBRC EQ 0.
            MESSAGE S151.
            RETURN.
          ENDIF.
        ENDIF.


    ENDCASE.

    R_VALIDADO = ABAP_TRUE.

  endmethod.
ENDCLASS.
