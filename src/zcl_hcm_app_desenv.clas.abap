class ZCL_HCM_APP_DESENV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_pa000x,
        pernr TYPE persno,
      END OF ty_pa000x .
  types:
    BEGIN OF ty_saidalv,
        icon           TYPE     char4,
        code           TYPE     pa0000-pernr,
        name           TYPE     pa0002-cname,
        login          TYPE     pa0465-cpf_nr,
        password       TYPE     pa0465-cpf_nr,
        status         TYPE     char1,
        email          TYPE     pa0105-usrid_long,
        phone          TYPE     char1,
        positioncode   TYPE     pa0001-stell,
        locationcode   TYPE     pa0001-kostl,
        stationcode    TYPE     char1,
        stationname    TYPE     char1,
        teamcode       TYPE     pa0001-orgeh,
        teamname       TYPE     hrp1000-stext,
        teamleadercode TYPE     pa9002-pernimed,
        profilecode    TYPE     char4,
      END OF ty_saidalv .
  types:
    tyt_saidalv TYPE STANDARD TABLE OF ty_saidalv .
  types:
    BEGIN OF ty_saida,
      code           TYPE     char10, "pa0000-pernr,
      name           TYPE     pa0002-cname,
      login          TYPE     pa0465-cpf_nr,
      password       TYPE     pa0465-cpf_nr,
      status         TYPE     char1,
      email          TYPE     string,
      phone          TYPE     char1,
      positioncode   TYPE     char10,
      locationcode   TYPE     pa0001-kostl,
      stationcode    TYPE     char1,
      stationname    TYPE     char1,
      teamcode       TYPE     char10, "pa0001-orgeh,
      teamname       TYPE     hrp1000-stext,
      teamleadercode TYPE     char10, "pa9002-pernimed,
*      profilecode    TYPE     char4,
      END OF ty_saida .
  types:
    tyt_saida TYPE STANDARD TABLE OF ty_saida .
  types:
    BEGIN OF ty_saidaorg,
             code        TYPE char10,
             name        TYPE char80,
             description TYPE char50,
             address     TYPE char50,
             document    TYPE char30,
             parentcode  TYPE char10,
           END OF ty_saidaorg .
  types:
    tyt_saidaorg TYPE STANDARD TABLE OF ty_saidaorg .

  constants GC_SERVICE type /UI2/SERVICE_NAME value 'DESENV_INT_ENVIA_USER' ##NO_TEXT.
  constants GC_ENVIADO type CHAR1 value 'S' ##NO_TEXT.
  data:
    ty_t_pa000x TYPE STANDARD TABLE OF ty_pa000x .
  data:
    gr_bukrs    TYPE RANGE OF bukrs .
  data GO_INTO type ref to ZCL_INT_ORG_DESENV .
  data GO_INT type ref to ZCL_INT_USER_DESENV .
  data:
    gr_kostl TYPE RANGE OF kostl .
  data:
    gr_pernr TYPE RANGE OF p_pernr .
  data:
    ty_pernr LIKE RANGE OF gr_pernr .
  data GV_ENDDA type SY-DATUM .
  data GV_CARLOC type FLAG .
  data GV_CARATI type FLAG .
  data GV_CARATU type FLAG .
  data GT_SAIDA type TYT_SAIDA .
  data GW_SAIDA type TY_SAIDA .
  data GW_SAIDAORG type TY_SAIDAORG .
  data GT_SAIDAORG type TYT_SAIDAORG .
  data GW_SAIDAC type TY_SAIDAORG .
  data GT_SAIDAC type TYT_SAIDAORG .
  data GW_SAIDACC type TY_SAIDAORG .
  data GT_SAIDACC type TYT_SAIDAORG .
  data GT_API type TYT_SAIDAORG .
  data GV_CARGOS type FLAG .

*  data GO_CARGO type ref to ZCL_INT_CARGO_DESENVI .
  methods RUN
    exporting
      !ET_RETURN type TYT_SAIDA
      !ET_RETURNO type TYT_SAIDAORG .
  methods SET_PARAM
    importing
      !IT_BUKRS type ANY TABLE
      !IT_KOSTL type ANY TABLE
      !IT_PERNR type ANY TABLE
      !I_ENDDA type SY-DATUM
      !I_CARLOC type FLAG
      !I_CARATI type FLAG
      !I_CARATU type FLAG
      !I_CARGOS type FLAG .
  methods GET_MATR_LAST_UPD
    importing
      !IV_DATA type SY-DATUM
    exporting
      !ET_PA000X like TY_T_PA000X .
  methods SET_API
    importing
      !IT_SAIDA type TYT_SAIDA
    exporting
      value(EV_ERROR) type CHAR1
      !EV_STAT type CHAR1
      !EV_CODE type STRING
      !EV_TEXT type STRING .
  methods GET_ORGANIZACAO
    exporting
      !EV_ERROR type CHAR1
      !EV_STAT type CHAR1
      !EV_CODE type STRING
      !EV_TEXT type STRING
      !ET_RETURNO type TYT_SAIDAORG .
  methods GET_CARGOS
    exporting
      value(ET_RETURNO) type TYT_SAIDAORG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HCM_APP_DESENV IMPLEMENTATION.


  METHOD get_cargos.


    SELECT objid AS code
           stext AS name
      FROM hrp1000
      INTO TABLE et_returno
      WHERE plvar EQ '01'
        AND otype EQ 'C'
        AND endda >= sy-datum
        AND langu EQ sy-langu.

    TRY .


        go_into = NEW #( ).

        IF go_into IS BOUND.
          go_into->zif_int_org_desenv~set_servico( i_servico = 'DESENV_INT_ENVIA_CARGO' ).
          DATA(lv_id_ret) = go_into->zif_int_org_desenv~enviar_desenvolve( EXPORTING it_saida  = et_returno ).
        ENDIF.
      CATCH zcx_error INTO DATA(ex_erro).

    ENDTRY.

  ENDMETHOD.


  METHOD get_matr_last_upd.
    DATA: lr_pernr TYPE RANGE OF char8.
    SELECT pernr
      FROM pa0465
      INTO TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    SELECT pernr
      FROM pa0000
      APPENDING TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    SELECT pernr
      FROM pa0002
      APPENDING TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    SELECT pernr
      FROM pa0105
      APPENDING TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    SELECT pernr
      FROM pa9002
      APPENDING TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    SELECT pernr
      FROM pa2001
      APPENDING TABLE @et_pa000x
     WHERE pernr IN @me->gr_pernr
        AND ( aedtm EQ @me->gv_endda
        OR    begda EQ @me->gv_endda
        OR    endda EQ @me->gv_endda ).

    IF et_pa000x[] IS NOT INITIAL.
      SORT et_pa000x BY pernr.
      DELETE ADJACENT DUPLICATES FROM et_pa000x COMPARING pernr.
    ENDIF.
  ENDMETHOD.


  METHOD get_organizacao.

    DATA: wa_api  TYPE ty_saidaorg,
          wa_apix TYPE ty_saidaorg.

*--------------------------------------------------------
* Seleção de dados
*--------------------------------------------------------
    SELECT a~kostl, a~bukrs, a~gsber, a~werks, b~ltext
      INTO TABLE @DATA(it_csks)
      FROM csks AS a
      INNER JOIN cskt AS b ON
                               a~kokrs  EQ b~kokrs
                           AND a~kostl  EQ b~kostl
                           AND a~datbi  EQ b~datbi
      WHERE a~bukrs IN @gr_bukrs
        AND a~kostl IN @gr_kostl
        AND a~datbi GE @gv_endda
        AND b~spras EQ @sy-langu.
    IF sy-subrc IS INITIAL.
      SORT it_csks BY bukrs gsber.

      DATA: t_set  TYPE TABLE OF rgsb4,
            wa_set TYPE rgsb4.
      DATA: lr_kostl TYPE RANGE OF kostl,
            ls_kostl LIKE LINE OF lr_kostl.
      DATA: v_check TYPE c VALUE ' '.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          client        = sy-mandt
          setnr         = 'MAGGI_CENTRO_APPDESENVOL'
          table         = 'CSKS'
          class         = '0000'
          fieldname     = 'KOSTL'
        TABLES
          set_values    = t_set
        EXCEPTIONS
          set_not_found = 1
          OTHERS        = 2.

      IF sy-subrc IS INITIAL.
        LOOP AT t_set INTO wa_set.

          IF wa_set-to NE wa_set-from.
            ls_kostl = VALUE #( sign = 'I' option = 'BT' low = wa_set-from high = wa_set-to ).
            APPEND ls_kostl TO lr_kostl.
            CLEAR ls_kostl.
          ELSE.
            ls_kostl = VALUE #( sign = 'I' option = 'EQ' low = wa_set-from ).
            APPEND ls_kostl TO lr_kostl.
            CLEAR ls_kostl.
          ENDIF.
        ENDLOOP.

        IF lr_kostl[] IS NOT INITIAL.
          DELETE it_csks WHERE kostl NOT IN lr_kostl.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK it_csks[] IS NOT INITIAL.

    SELECT bukrs, name, branch, stcd1, cgc_branch
      FROM j_1bbranch
      INTO TABLE @DATA(it_branch)
      FOR ALL ENTRIES IN @it_csks
      WHERE bukrs  EQ @it_csks-bukrs
        AND branch EQ @it_csks-gsber.
    IF sy-subrc IS INITIAL.
      SORT it_branch BY bukrs branch.
    ENDIF.
*Buscar nome empresa
    SELECT bukrs, butxt
      FROM t001
      INTO TABLE @DATA(it_t001)
      FOR ALL ENTRIES IN @it_csks
      WHERE bukrs EQ @it_csks-bukrs.
    IF sy-subrc IS INITIAL.
      SORT it_t001 BY bukrs.
    ENDIF.
*
*Buscar nome da filial
    SELECT werks, name1, stras, ort01, pstlz
      FROM t001w
      INTO TABLE @DATA(it_t001w)
     FOR ALL ENTRIES IN @it_csks
    WHERE werks EQ @it_csks-gsber.
    IF sy-subrc IS INITIAL.
      SORT it_t001w BY werks.
    ENDIF.

    LOOP AT it_csks ASSIGNING FIELD-SYMBOL(<fs_csks>).

      READ TABLE it_branch INTO DATA(wa_branch)
                           WITH KEY bukrs = <fs_csks>-bukrs
                                    branch = <fs_csks>-gsber
                           BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        MOVE: <fs_csks>-bukrs TO gw_saidaorg-code.

        MOVE: <fs_csks>-gsber TO gw_saidac-code,
              wa_branch-stcd1 TO gw_saidac-document,
              <fs_csks>-bukrs TO gw_saidac-parentcode.

        MOVE: <fs_csks>-kostl TO gw_saidacc-code,
              <fs_csks>-ltext TO gw_saidacc-name,
              <fs_csks>-gsber TO gw_saidacc-parentcode.

      ENDIF.

      READ TABLE it_t001 INTO DATA(wa_t001)
                           WITH KEY bukrs = <fs_csks>-bukrs
                           BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE  wa_t001-butxt TO gw_saidaorg-name.
      ENDIF.

      READ TABLE it_t001w INTO DATA(wa_t001w)
                           WITH KEY werks = <fs_csks>-gsber
                           BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE: wa_t001w-name1 TO gw_saidac-name.
        CONCATENATE wa_t001w-stras
                    wa_t001w-ort01
                    wa_t001w-pstlz
                    INTO gw_saidac-address
                    SEPARATED BY space.
      ENDIF.

      APPEND gw_saidaorg TO gt_saidaorg.
      CLEAR  gw_saidaorg.
      APPEND gw_saidac TO gt_saidac.
      CLEAR  gw_saidac.
      APPEND gw_saidacc TO gt_saidacc.
      CLEAR  gw_saidacc.
    ENDLOOP.

    SORT: gt_saidaorg BY code name description address document parentcode,
          gt_saidac   BY code name description address document parentcode,
          gt_saidacc  BY code name description address document parentcode.

    DELETE ADJACENT DUPLICATES FROM: gt_saidaorg COMPARING ALL FIELDS,
                                     gt_saidac   COMPARING ALL FIELDS,
                                     gt_saidacc  COMPARING ALL FIELDS.

    DELETE:  gt_saidaorg WHERE code IS INITIAL,
             gt_saidac   WHERE code IS INITIAL,
             gt_saidacc  WHERE code IS INITIAL.

    SORT: gt_saidaorg BY code,
          gt_saidac   BY parentcode,
          gt_saidacc  BY parentcode.

*--------------------------------------------------------
* Enviar
*--------------------------------------------------------
*Primeiro enviar todas bukrs
    LOOP AT gt_saidaorg ASSIGNING FIELD-SYMBOL(<fs_sorg>).

      MOVE: <fs_sorg> TO wa_api,
            ''    TO wa_api-parentcode.
      APPEND wa_api TO gt_api.
      FREE wa_api.

      READ TABLE gt_saidac WITH KEY parentcode = <fs_sorg>-code
                            TRANSPORTING NO FIELDS
                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(lv_cx) = sy-tabix.

        LOOP AT gt_saidac ASSIGNING FIELD-SYMBOL(<fs_sc>) FROM lv_cx.

          IF <fs_sorg>-code NE <fs_sc>-parentcode.
            EXIT.
          ELSE.

            MOVE: <fs_sc> TO wa_api.
            APPEND wa_api TO gt_api.
            FREE wa_api.

            READ TABLE gt_saidacc WITH KEY parentcode = <fs_sc>-code
                                  TRANSPORTING NO FIELDS
                                  BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              DATA(lv_ccx) = sy-tabix.
              LOOP AT gt_saidacc ASSIGNING FIELD-SYMBOL(<fs_scc>) FROM lv_ccx.
                IF <fs_scc>-parentcode NE <fs_sc>-code.
                  EXIT.
                ELSE.
                  MOVE <fs_scc> TO wa_api.
                  APPEND wa_api TO gt_api.
                  FREE wa_api.
                ENDIF.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

*    wa_apix-code  = '9999'.
*    wa_apix-name  = 'GRUPO MAGGI'.
*    APPEND wa_apix TO gt_api.
    CLEAR wa_apix.

    IF gt_api[] IS NOT INITIAL.
      TRY.
          go_into = NEW #( ).

          IF go_into IS BOUND.
            DATA(lv_id_ret) = go_into->zif_int_org_desenv~enviar_desenvolve( EXPORTING it_saida        = gt_api
                                                                             IMPORTING ev_return_code  = ev_code
                                                                                       ev_return_msg   = ev_text ).
          ENDIF.

        CATCH zcx_integracao INTO DATA(ex_int).
          ev_text = ex_int->get_longtext( ).
        CATCH zcx_error INTO DATA(ex_erro).
          ev_text = ex_erro->get_longtext( ).
      ENDTRY.

      et_returno[] = gt_api[].

    ENDIF.

  ENDMETHOD.


  METHOD run.
    DATA: lo_sap_hcm TYPE REF TO zcl_hcm_util.
    DATA:
      lr_pa000x TYPE RANGE OF p_pernr,
      lw_pa000x LIKE LINE OF lr_pa000x.

    TRY .


*------------------------------------------------
* Carga Locais (Estrutura Empresas)
*------------------------------------------------
        IF me->gv_carloc IS NOT INITIAL.
          me->get_organizacao( IMPORTING et_returno = et_returno ). " return get api
        ENDIF.

*------------------------------------------------
* Anvia Cargos
*------------------------------------------------
        IF me->gv_cargos IS NOT INITIAL.
          me->get_cargos( IMPORTING et_returno = et_returno ).
        ENDIF.

        lo_sap_hcm = NEW #( ).
*------------------------------------------------
* Carga Ativos
*------------------------------------------------

        IF me->gv_carati IS NOT INITIAL.

* GET funcionários (Ativos... Afastados)
          lo_sap_hcm->get_funcionarios_app_desenv( EXPORTING ir_bukrs = me->gr_bukrs
                                                  ir_pernr = me->gr_pernr
                                                  ir_kostl = me->gr_kostl
                                                  iv_endda = me->gv_endda
                                        IMPORTING et_return = gt_saida ).

        ENDIF.
*------------------------------------------------
* Carga Atualização
*------------------------------------------------
        IF me->gv_caratu IS NOT INITIAL.

* Buscar matriculas válidas para atualização, auando a opção for Carga Atualização...
          me->get_matr_last_upd( EXPORTING iv_data = me->gv_endda
                                 IMPORTING et_pa000x = DATA(it_pa000x) ).

          IF it_pa000x[] IS NOT INITIAL.
* Colocar na lista
            LOOP AT it_pa000x INTO DATA(wa_pa000x).
              lw_pa000x = VALUE #( sign = 'I' option = 'EQ' low = wa_pa000x-pernr ).
              APPEND lw_pa000x TO lr_pa000x.
              CLEAR lw_pa000x.
            ENDLOOP.

            lo_sap_hcm->get_funcionarios_app_desenv( EXPORTING ir_bukrs = me->gr_bukrs
                                                    ir_pernr = lr_pa000x
                                                    ir_kostl = me->gr_kostl
                                                    iv_endda = me->gv_endda
                                                    iv_upd   = abap_on
                                          IMPORTING et_return = gt_saida ).


          ENDIF.
        ENDIF.

        SORT gt_saida BY code.
        DELETE ADJACENT DUPLICATES FROM gt_saida COMPARING code.
        et_return[] = gt_saida[].

      CATCH zcx_error INTO DATA(ex_erro).

    ENDTRY.

  ENDMETHOD.


  METHOD set_api.
    CHECK it_saida IS NOT INITIAL.
    ev_error = abap_true. "<- se nao for erro, vai ser limpado

    TRY.
        go_int = NEW #( ).

        IF go_int IS BOUND.
          DATA(lv_id_ret) = go_int->zif_int_user_desenv~enviar_desenvolve( EXPORTING it_saida = it_saida
                                                                           IMPORTING ev_return_code = ev_code
                                                                                     ev_return_msg = ev_text ).
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        ev_text = ex_int->get_longtext( ).
      CATCH zcx_error INTO DATA(ex_erro).
        ev_text = ex_erro->get_longtext( ).
    ENDTRY.

  ENDMETHOD.


  METHOD set_param.
    gr_bukrs  = it_bukrs.
    gr_kostl  = it_kostl.
    gr_pernr  = it_pernr.
    gv_endda  = i_endda.
    gv_carloc = i_carloc.
    gv_carati = i_carati.
    gv_caratu = i_caratu.
    gv_cargos = i_cargos.
  ENDMETHOD.
ENDCLASS.
