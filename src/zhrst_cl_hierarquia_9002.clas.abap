class ZHRST_CL_HIERARQUIA_9002 definition
  public
  final
  create public .

public section.

  data ATR_CHEFE012 type PERSNO .
  data ATR_ORGEH012 type ORGEH .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZHRST_CL_HIERARQUIA_9002 .
  methods GET_GESTORES
    importing
      !I_PERNR type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002
    raising
      ZCX_EXCEPTION_HIERARQUIA .
  methods ZM_GET_GESTOR_IMED
    importing
      value(I_PERNR) type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
  methods ZM_GET_GESTOR_MED
    importing
      !I_PERNR type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
  methods ZM_GET_GESTOR_MATR
    importing
      value(I_PERNR) type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
  methods ZM_GET_GESTOR_ALCA
    importing
      value(I_PERNR) type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
  methods ZM_GET_GESTOR_IMED_UP
    importing
      !I_PERNR type PERSNO optional
      !I_ORGEH type ORGEH optional
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
  methods ZM_GET_GESTOR_MATR_S_S
    importing
      value(I_PERNR) type PERNR_D
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    changing
      !C_P9002 type P9002 .
protected section.
private section.

  class-data ATR_HIERARQUIA_9002 type ref to ZHRST_CL_HIERARQUIA_9002 .
ENDCLASS.



CLASS ZHRST_CL_HIERARQUIA_9002 IMPLEMENTATION.


  METHOD GET_GESTORES.

    CLEAR : ME->ATR_CHEFE012,ME->ATR_ORGEH012.

*--Buscar dados gestor imediato (012)
    ME->ZM_GET_GESTOR_IMED( EXPORTING I_PERNR = I_PERNR
                                      I_BEGDA = I_BEGDA
                                      I_ENDDA = I_ENDDA
                            CHANGING  C_P9002 = C_P9002   ).

*--Buscar dados gestor mediato ( orgeh acima )
    ME->ZM_GET_GESTOR_MED( EXPORTING I_PERNR = I_PERNR
                                     I_BEGDA = I_BEGDA
                                     I_ENDDA = I_ENDDA
                           CHANGING  C_P9002 = C_P9002   ).


*--Buscar dados gestor matricial
    ME->ZM_GET_GESTOR_MATR( EXPORTING I_PERNR = I_PERNR
                                      I_BEGDA = I_BEGDA
                                      I_ENDDA = I_ENDDA
                            CHANGING  C_P9002 = C_P9002   ).

*--Buscar dados gestor matricial
    ME->ZM_GET_GESTOR_ALCA( EXPORTING I_PERNR = I_PERNR
                                      I_BEGDA = I_BEGDA
                                      I_ENDDA = I_ENDDA
                            CHANGING  C_P9002 = C_P9002   ).

  ENDMETHOD.


  METHOD GET_INSTANCE.
    IF ZHRST_CL_HIERARQUIA_9002=>ATR_HIERARQUIA_9002 IS NOT BOUND.
      CREATE OBJECT ZHRST_CL_HIERARQUIA_9002=>ATR_HIERARQUIA_9002.
    ENDIF.

    R_INSTANCE = ZHRST_CL_HIERARQUIA_9002=>ATR_HIERARQUIA_9002.

  ENDMETHOD.


  METHOD zm_get_gestor_alca.
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_subty1)   = CONV subty('0001' ).

    CLEAR : c_p9002-pernalca, c_p9002-cnamealca, c_p9002-begalca, c_p9002-endalca, c_p9002-gestalca.
*--Ler ZHRST_9002_ALCAD / Read ZHRST_9002_ALCAD

    SELECT * INTO TABLE @DATA(lt_9002_alcad)
      FROM zhrst_9002_alcad
      WHERE pernr = @i_pernr
      AND begda <=  @i_begda
      AND endda >=  @i_begda.

    IF line_exists( lt_9002_alcad[ 1 ] ).

      READ TABLE lt_9002_alcad INTO DATA(ls_alcad) INDEX 1.

      c_p9002-pernalca = ls_alcad-pernr_alca.

      CHECK NOT c_p9002-pernalca CO '0 '.
      "Nome do Gestor matricial
      SELECT SINGLE cname
       FROM pa0002
       INTO @DATA(lo_cname_alca)
        WHERE pernr EQ @c_p9002-pernalca
        AND endda >= @sy-datum. " BUG - 83805 - Quando o gestor muda de nome.

      c_p9002-cnamealca = lo_cname_alca.
      c_p9002-begalca   = ls_alcad-begda.
      c_p9002-endalca   = ls_alcad-endda.

      "CPF do Gestor matricial
      SELECT SINGLE cpf_nr
       FROM pa0465
       INTO @DATA(lo_cpf_nr)
        WHERE pernr EQ @c_p9002-pernalca
        AND   subty EQ @lo_lv_subty1.

      c_p9002-gestalca = lo_cpf_nr.

    ELSE.

      c_p9002-cnamealca = 'Sem Delegação '.

    ENDIF.
  ENDMETHOD.


  METHOD zm_get_gestor_imed.
    DATA(lo_lv_plvar01)  = CONV plvar('01').
    DATA(lo_lv_otypeo)   = CONV otype('O').
    DATA(lo_lv_istat1)   = CONV istat_d('1').
    DATA(lo_lv_b012)     = CONV subty('B012' ).
    DATA(lo_lv_b003)     = CONV subty('B003' ).
    DATA(lo_lv_subty1)   = CONV subty('0001' ).
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_objid)    = VALUE realo( ).
    DATA(lo_lv_pernr)    = VALUE realo( ).
    DATA(lo_lv_sclass)   = CONV sclas('S').
    DATA(lo_lv_plans)    = VALUE sobid( ).

    DATA: lo_lt_p0001 TYPE TABLE OF p0001.
    DATA: lo_lt_p1001 TYPE TABLE OF p1001.

    CLEAR : c_p9002-pernimed, c_p9002-cnameimed, c_p9002-begimed, c_p9002-endimed, c_p9002-gestimed.
*--Ler IT0001 / Read IT0001 do pernr do it9002

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lo_lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    SORT lo_lt_p0001 BY endda DESCENDING.

    READ TABLE lo_lt_p0001 INTO DATA(ls_p0001) INDEX 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    lo_lv_objid = ls_p0001-orgeh.
    lo_lv_plans = ls_p0001-plans.

    CALL FUNCTION 'RH_GET_LEADER'
      EXPORTING
        plvar                     = lo_lv_plvar01
        keydate                   = i_begda
        otype                     = lo_lv_otypeo
        objid                     = lo_lv_objid
        get_leader_tab            = abap_true
      IMPORTING
        leader_id                 = lo_lv_pernr
      EXCEPTIONS
        no_leader_found           = 1
        no_leading_position_found = 2
        OTHERS                    = 3.

*    IF SY-SUBRC <> 0.
*
*      RETURN.
*
*    ENDIF.
*--Checar se pernr é chefe dele mesmo ( B012)
    IF ( lo_lv_pernr = i_pernr OR sy-subrc <> 0 ).

*--Buscar o chefe da orgeh acima
      DATA(lo_p9002_up) = VALUE p9002( ).
      me->zm_get_gestor_imed_up( EXPORTING i_pernr = i_pernr
                                           i_orgeh = ls_p0001-orgeh
                                           i_begda = i_begda
                                           i_endda = i_endda
                                  CHANGING c_p9002 = lo_p9002_up   ).
*--Dados do chefe ( 012 ) da Unidade acima ( imediato = mediato )
      c_p9002-pernimed  = lo_p9002_up-pernmed. "Pernr do chefe acima ( orgeh )
      c_p9002-cnameimed = lo_p9002_up-cnamemed.
      c_p9002-gestimed  = lo_p9002_up-gestmed.
      c_p9002-begimed   = i_begda.
      c_p9002-endimed   = i_endda.
      me->atr_chefe012  = c_p9002-pernimed.
      me->atr_orgeh012  = ls_p0001-orgeh.
      RETURN.

    ENDIF.
    c_p9002-pernimed = lo_lv_pernr. "Procurar pelo pernr do gestor

    "Nome do Gestor imediato
    SELECT SINGLE cname
     FROM pa0002
     INTO @DATA(lo_cname_imed)
      WHERE pernr EQ @c_p9002-pernimed
      AND endda >= @sy-datum. " BUG - 83805 - Quando o gestor muda de nome

    c_p9002-cnameimed = lo_cname_imed.

    "CPF do Gestor imediato
    SELECT SINGLE cpf_nr
     FROM pa0465
     INTO @DATA(lo_cpf_nr)
      WHERE pernr EQ @c_p9002-pernimed
      AND   subty EQ @lo_lv_subty1.

    c_p9002-gestimed = lo_cpf_nr.
    c_p9002-begimed  = i_begda.
    c_p9002-endimed  = i_endda.

  ENDMETHOD.


  METHOD zm_get_gestor_imed_up.

    DATA(lo_lv_plvar01)  = CONV plvar('01').
    DATA(lo_lv_otypeo)   = CONV otype('O').
    DATA(lo_lv_istat1)   = CONV istat_d('1').
    DATA(lo_lv_b012)     = CONV subty('B012' ).
    DATA(lo_lv_b003)     = CONV subty('B003' ).
    DATA(lo_lv_subty1)   = CONV subty('0001' ).
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_objid)    = VALUE realo( ).
    DATA(lo_lv_pernr)    = VALUE realo( ).
    DATA(lo_root_objec)  = VALUE hrrootob( ).
    DATA(lo_lv_sclass)   = CONV sclas('S').
    DATA(lo_pernr)       = VALUE persno( ).
    DATA(lo_orgeh)       = VALUE orgeh( ).
    DATA: lo_lt_p0001 TYPE TABLE OF p0001.
    DATA: lo_lt_p1001 TYPE TABLE OF p1001.

    DATA lt_result_objec TYPE STANDARD TABLE OF objec.
    DATA lt_result_struc TYPE STANDARD TABLE OF struc.
    DATA lt_root_objects TYPE STANDARD TABLE OF hrrootob.

    CLEAR : c_p9002-pernmed, c_p9002-cnamemed, c_p9002-begmed, c_p9002-endmed, c_p9002-gestmed.

    lo_lv_objid         = i_orgeh.
    lo_root_objec-otype = lo_lv_otypeo.
    lo_root_objec-objid = i_orgeh.

    APPEND lo_root_objec TO lt_root_objects.

    DATA(lo_orga_up) = CONV wegid('ORGA-UP').
    DATA(lo_tdepth3) = CONV tdepth('10').  "Nível técnico da estrutura

    CALL FUNCTION 'RH_STRUC_GET_MULTIPLE_ROOTS'
      EXPORTING
        act_wegid            = lo_orga_up
        act_int_flag         = abap_false
        act_plvar            = lo_lv_plvar01
        act_begda            = i_begda
        act_endda            = i_endda
        act_tdepth           = lo_tdepth3
        act_tflag            = abap_true
        act_vflag            = abap_true
        act_sflag            = abap_true
        act_recurs           = abap_true
        act_text_buffer_fill = abap_true
        authority_check      = abap_false
      TABLES
        root_objects         = lt_root_objects
        result_objec         = lt_result_objec
        result_struc         = lt_result_struc
      EXCEPTIONS
        no_plvar_found       = 1
        no_entry_found       = 2
        path_not_found       = 3
        root_not_found       = 4
        OTHERS               = 5.

    LOOP AT lt_result_objec INTO DATA(ls_result_objec).
      "CHECK LS_RESULT_OBJEC-OBJID NE I_ORGEH.
      IF ( ls_result_objec-objid EQ i_orgeh ).
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'RH_GET_LEADER'
        EXPORTING
          plvar                     = lo_lv_plvar01
          keydate                   = i_begda
          otype                     = lo_lv_otypeo
          objid                     = ls_result_objec-realo
          get_leader_tab            = abap_true
        IMPORTING
          leader_id                 = lo_lv_pernr
        EXCEPTIONS
          no_leader_found           = 1
          no_leading_position_found = 2
          OTHERS                    = 3.

      IF sy-subrc = 0 AND NOT lo_lv_pernr CO '0 '.

        c_p9002-pernmed = lo_lv_pernr.

        "Nome do Gestor mediato
        SELECT SINGLE cname
         FROM pa0002
         INTO @DATA(lo_cname_med)
          WHERE pernr EQ @c_p9002-pernmed
            AND endda >= @sy-datum.

        c_p9002-cnamemed = lo_cname_med.

        "CPF do Gestor mediato
        SELECT SINGLE cpf_nr
         FROM pa0465
         INTO @DATA(lo_cpf_nr)
          WHERE pernr EQ @c_p9002-pernmed
          AND   subty EQ @lo_lv_subty1.

        c_p9002-gestmed = lo_cpf_nr.
        c_p9002-begmed = i_begda.
        c_p9002-endmed = i_endda.

*        LO_ORGEH        = LS_RESULT_OBJEC-REALO.
        EXIT.
      ENDIF.

    ENDLOOP.

    "    CHECK NOT  C_P9002-PERNMED CO '0 '.

  ENDMETHOD.


  METHOD zm_get_gestor_matr.
    DATA(lo_lv_plvar01)  = CONV plvar('01').
    DATA(lo_lv_otypeo)   = CONV otype('O').
    DATA(lo_lv_otypes)   = CONV otype('S').
    DATA(lo_lv_istat1)   = CONV istat_d('1').
    DATA(lo_lv_a008)     = CONV subty('A008' ).
    DATA(lo_lv_bz02)     = CONV subty('BZ02' ).
    DATA(lo_lv_az02)     = CONV subty('AZ02' ). "Ler de O para S AZ02
    DATA(lo_lv_subty1)   = CONV subty('0001' ).
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_plans)    = VALUE plans( ).
    DATA(lo_lv_sclasp)   = CONV sclas('P').
    DATA(lo_lv_sclass)   = CONV sclas('S').
    DATA: lo_lt_p0001  TYPE TABLE OF p0001.
    DATA: lo_lt_p1001p TYPE TABLE OF p1001.
    DATA: lo_lt_p1001  TYPE TABLE OF p1001.

    CLEAR : c_p9002-pernmatr, c_p9002-cnamematr, c_p9002-begmatr, c_p9002-endmatr, c_p9002-gestmatr.
*--Procurar a ligação S x S BZ02
*--Buscar dados gestor matricial
    me->zm_get_gestor_matr_s_s( EXPORTING i_pernr = i_pernr
                                          i_begda = i_begda
                                          i_endda = i_endda
                                CHANGING  c_p9002 = c_p9002   ).

    CHECK c_p9002-pernmatr CO '0 '.

*--Ler IT0001 / Read IT0001
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lo_lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    SORT lo_lt_p0001 BY endda DESCENDING.

    READ TABLE lo_lt_p0001 INTO DATA(ls_p0001) INDEX 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

*--Procurar ligação O x S
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar            = lo_lv_plvar01
        otype            = lo_lv_otypeo
        objid            = ls_p0001-orgeh
        subty            = lo_lv_az02 "Ler de O para S AZ02
*       BEGDA            = I_BEGDA
*       ENDDA            = I_ENDDA
      TABLES
        i1001            = lo_lt_p1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.

    SORT lo_lt_p1001 BY endda DESCENDING.

    READ TABLE lo_lt_p1001 INTO DATA(ls_p1001) INDEX 1.
* Reading of the position.

    IF sy-subrc = 0.
      lo_lv_plans     = ls_p1001-sobid. "Buscar Pernr pela Plans
      c_p9002-begmatr = i_begda.
      c_p9002-endmatr = i_endda.

      CALL FUNCTION 'RH_READ_INFTY_1001'
        EXPORTING
          plvar            = lo_lv_plvar01
          otype            = lo_lv_otypes
          objid            = lo_lv_plans
          subty            = lo_lv_a008
*         BEGDA            = I_BEGDA
*         ENDDA            = I_ENDDA
        TABLES
          i1001            = lo_lt_p1001p
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.

      SORT lo_lt_p1001p BY endda DESCENDING.
* Reading of the pernr ( sclas = P ).
      READ TABLE lo_lt_p1001p INTO DATA(ls_p1001p) WITH KEY sclas = lo_lv_sclasp.

      c_p9002-pernmatr = ls_p1001p-sobid.

    ENDIF.

    CHECK NOT c_p9002-pernmatr CO '0 '.
    "Nome do Gestor matricial
    SELECT SINGLE cname
     FROM pa0002
     INTO @DATA(lo_cname_matr)
      WHERE pernr EQ @c_p9002-pernmatr
      AND endda >= @sy-datum. " BUG - 83805 - Quando o gestor muda de nome.

    c_p9002-cnamematr = lo_cname_matr.

    "CPF do Gestor matricial
    SELECT SINGLE cpf_nr
     FROM pa0465
     INTO @DATA(lo_cpf_nr)
      WHERE pernr EQ @c_p9002-pernmatr
      AND   subty EQ @lo_lv_subty1.

    c_p9002-gestmatr = lo_cpf_nr.

  ENDMETHOD.


  METHOD zm_get_gestor_matr_s_s.
    DATA(lo_lv_plvar01)  = CONV plvar('01').
    DATA(lo_lv_otypeo)   = CONV otype('O').
    DATA(lo_lv_otypes)   = CONV otype('S').
    DATA(lo_lv_istat1)   = CONV istat_d('1').
    DATA(lo_lv_a008)     = CONV subty('A008' ).
    DATA(lo_lv_bz02)     = CONV subty('BZ02' ).
    DATA(lo_lv_az02)     = CONV subty('AZ02' ). "Ler de O para S AZ02
    DATA(lo_lv_subty1)   = CONV subty('0001' ).
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_plans)    = VALUE plans( ).
    DATA(lo_lv_sclasp)   = CONV sclas('P').
    DATA(lo_lv_sclass)   = CONV sclas('S').
    DATA: lo_lt_p0001  TYPE TABLE OF p0001.
    DATA: lo_lt_p1001p TYPE TABLE OF p1001.
    DATA: lo_lt_p1001  TYPE TABLE OF p1001.

    CLEAR : c_p9002-pernmatr, c_p9002-cnamematr, c_p9002-begmatr, c_p9002-endmatr, c_p9002-gestmatr.
*--Ler IT0001 / Read IT0001
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lo_lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    SORT lo_lt_p0001 BY endda DESCENDING.

    READ TABLE lo_lt_p0001 INTO DATA(ls_p0001) INDEX 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
*--Procurar a ligação S( Colaborador ) x S (Gestor ) AZ02
*--Partir do S lido pelo pernr da pa30 procurar pela ligação AZ02
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar            = lo_lv_plvar01
        otype            = lo_lv_otypes
        objid            = ls_p0001-plans
        subty            = lo_lv_az02
*       BEGDA            = I_BEGDA
*       ENDDA            = I_ENDDA
      TABLES
        i1001            = lo_lt_p1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.

    SORT lo_lt_p1001 BY endda DESCENDING.
*--Validar com a data do infotipo que esta sendo criado
*    01.04.2020 pa30  hrp1001 - s x 0 01.04.2020 pernr esperado 70001129 TabelaSalarial Amaggi Teste
*    01.05.2020 pa 30 hrp1001 - s x s 01.05.2020 pernr esperado 70001132 Tabela Salarial TESTE

    READ TABLE lo_lt_p1001 INTO DATA(ls_p1001) INDEX 1.

    IF ls_p1001-begda > i_begda.
      RETURN.
    ENDIF.

* Reading of the position.

    IF sy-subrc = 0.
      lo_lv_plans     = CONV plans( ls_p1001-sobid ). "Buscar Pernr pela Plans do sobid
      c_p9002-begmatr = i_begda.
      c_p9002-endmatr = i_endda.

      CALL FUNCTION 'RH_READ_INFTY_1001'
        EXPORTING
          plvar            = lo_lv_plvar01
          otype            = lo_lv_otypes
          objid            = lo_lv_plans
          subty            = lo_lv_a008
*         BEGDA            = I_BEGDA
*         ENDDA            = I_ENDDA
        TABLES
          i1001            = lo_lt_p1001p
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.

      SORT lo_lt_p1001p BY endda DESCENDING.
* Reading of the pernr ( sclas = P ).
      READ TABLE lo_lt_p1001p INTO DATA(ls_p1001p) WITH KEY sclas = lo_lv_sclasp.

      c_p9002-pernmatr = ls_p1001p-sobid.

    ENDIF.

    CHECK NOT c_p9002-pernmatr CO '0 '.
    "Nome do Gestor matricial
    SELECT SINGLE cname
     FROM pa0002
     INTO @DATA(lo_cname_matr)
      WHERE pernr EQ @c_p9002-pernmatr
      AND endda >= @sy-datum. " BUG - 83805 - Quando o gestor muda de nome.

    c_p9002-cnamematr = lo_cname_matr.

    "CPF do Gestor matricial
    SELECT SINGLE cpf_nr
     FROM pa0465
     INTO @DATA(lo_cpf_nr)
      WHERE pernr EQ @c_p9002-pernmatr
      AND   subty EQ @lo_lv_subty1.

    c_p9002-gestmatr = lo_cpf_nr.

  ENDMETHOD.


  METHOD zm_get_gestor_med.

*--Ler IT0001 / Read IT0001
    DATA(lo_lv_plvar01)  = CONV plvar('01').
    DATA(lo_lv_otypeo)   = CONV otype('O').
    DATA(lo_lv_istat1)   = CONV istat_d('1').
    DATA(lo_lv_b012)     = CONV subty('B012' ).
    DATA(lo_lv_b003)     = CONV subty('B003' ).
    DATA(lo_lv_subty1)   = CONV subty('0001' ).
    DATA(lo_lv_high)     = CONV endda( '99991231' ).
    DATA(lo_lv_objid)    = VALUE realo( ).
    DATA(lo_lv_pernr)    = VALUE realo( ).
    DATA(lo_root_objec)  = VALUE hrrootob( ).
    DATA(lo_lv_sclass)   = CONV sclas('S').
    DATA(lo_pernr)       = VALUE persno( ).
    DATA(lo_orgeh)       = VALUE orgeh( ).
    DATA: lo_lt_p0001 TYPE TABLE OF p0001.
    DATA: lo_lt_p1001 TYPE TABLE OF p1001.

    DATA lt_result_objec TYPE STANDARD TABLE OF objec.
    DATA lt_result_struc TYPE STANDARD TABLE OF struc.
    DATA lt_root_objects TYPE STANDARD TABLE OF hrrootob.

    CLEAR : c_p9002-pernmed, c_p9002-cnamemed, c_p9002-begmed, c_p9002-endmed, c_p9002-gestmed.
*--Ler IT0001 / Read IT0001
    IF me->atr_chefe012 CO '0 '.
      lo_pernr = i_pernr.

    ELSE.
      lo_pernr = me->atr_chefe012.
      lo_orgeh = me->atr_orgeh012.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lo_lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    SORT lo_lt_p0001 BY endda DESCENDING.

    READ TABLE lo_lt_p0001 INTO DATA(ls_p0001)  WITH KEY pernr = i_pernr
                                                         endda = lo_lv_high.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    lo_lv_objid         = lo_orgeh.
    lo_root_objec-otype = lo_lv_otypeo.
    lo_root_objec-objid = ls_p0001-orgeh.

    APPEND lo_root_objec TO lt_root_objects.

    DATA(lo_orga_up) = CONV wegid('ORGA-UP').
    DATA(lo_tdepth3) = CONV tdepth('7').  "Nível técnico da estrutura

    CALL FUNCTION 'RH_STRUC_GET_MULTIPLE_ROOTS'
      EXPORTING
        act_wegid            = lo_orga_up
        act_int_flag         = abap_false
        act_plvar            = lo_lv_plvar01
        act_begda            = i_begda
        act_endda            = i_endda
        act_tdepth           = lo_tdepth3
        act_tflag            = abap_true
        act_vflag            = abap_true
        act_sflag            = abap_true
        act_recurs           = abap_true
        act_text_buffer_fill = abap_true
        authority_check      = abap_false
      TABLES
        root_objects         = lt_root_objects
        result_objec         = lt_result_objec
        result_struc         = lt_result_struc
      EXCEPTIONS
        no_plvar_found       = 1
        no_entry_found       = 2
        path_not_found       = 3
        root_not_found       = 4
        OTHERS               = 5.

    LOOP AT lt_result_objec INTO DATA(ls_result_objec).
      CHECK ls_result_objec-objid NE ls_p0001-orgeh.

      CALL FUNCTION 'RH_GET_LEADER'
        EXPORTING
          plvar                     = lo_lv_plvar01
          keydate                   = i_begda
          otype                     = lo_lv_otypeo
          objid                     = ls_result_objec-realo
          get_leader_tab            = abap_true
        IMPORTING
          leader_id                 = lo_lv_pernr
        EXCEPTIONS
          no_leader_found           = 1
          no_leading_position_found = 2
          OTHERS                    = 3.

      IF sy-subrc = 0 AND NOT lo_lv_pernr CO '0 ' AND lo_lv_pernr <> c_p9002-pernimed.

        IF c_p9002-pernimed NE lo_lv_pernr.
          c_p9002-pernmed = lo_lv_pernr.

          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CHECK NOT  c_p9002-pernmed CO '0 '.

    "Nome do Gestor mediato
    SELECT SINGLE cname
     FROM pa0002
     INTO @DATA(lo_cname_med)
      WHERE pernr EQ @c_p9002-pernmed
      AND endda >= @sy-datum. " BUG - 83805 - Quando o gestor muda de nome.

    c_p9002-cnamemed = lo_cname_med.

    "CPF do Gestor mediato
    SELECT SINGLE cpf_nr
     FROM pa0465
     INTO @DATA(lo_cpf_nr)
      WHERE pernr EQ @c_p9002-pernmed
      AND   subty EQ @lo_lv_subty1.

    c_p9002-gestmed = lo_cpf_nr.
    c_p9002-begmed = i_begda.
    c_p9002-endmed = i_endda.

  ENDMETHOD.
ENDCLASS.
