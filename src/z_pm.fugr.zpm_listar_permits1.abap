FUNCTION zpm_listar_permits1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     REFERENCE(I_AUFNR) TYPE  AUFK-AUFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LINES) TYPE  NUMC2
*"  TABLES
*"      T_ORDENS STRUCTURE  ZPMR0003 OPTIONAL
*"----------------------------------------------------------------------

* Declaração dos tipos de dados globais
  TYPES:   BEGIN OF ty_ordens.
             INCLUDE  TYPE zpmr0003.
  TYPES:    END OF ty_ordens,

  BEGIN OF ty_ihsg,
    objnr       TYPE  ihsg-objnr,
    counter     TYPE  ihsg-counter,
    ernam       TYPE  ihsg-ernam,
    pmsog       TYPE  ihsg-pmsog,
    nivel       TYPE  zpmr0002-nivel,
    aprovador   TYPE  zpmr0002-aprovador,
    usua_subst  TYPE  zpmr0002-usua_subst,
    data_lim    TYPE  zpmr0002-data_lim,
    check(1),
    centro_desp TYPE zpmr0002-centro_desp,
  END OF ty_ihsg.

*** Tabelas
  DATA: gt_ordens     TYPE TABLE OF ty_ordens,
        gt_ordens_aux TYPE TABLE OF ty_ordens WITH HEADER LINE,
        it_ordens     TYPE TABLE OF ty_ordens,
        t_ordens_aux  TYPE TABLE OF zpmr0003,
        it_ihsg       TYPE TABLE OF ty_ihsg,
        lw_ordens     TYPE zpmr0003,
        objnr_        TYPE j_objnr,
        objnr_aux     TYPE j_objnr,
        nivel_        TYPE numc10,
        gt_zpmr0002   TYPE TABLE OF zpmr0002 WITH HEADER LINE,
        zt_zpmr0002   TYPE TABLE OF zpmr0002 WITH HEADER LINE,
        gt_zpmr0011   TYPE TABLE OF zpmr0011 WITH HEADER LINE,
        order_types   TYPE rsis_t_range,
        r_werks       TYPE rsis_t_range,
        rg_aufnr_e    TYPE RANGE OF aufnr,
        rg_aufnr_d    TYPE RANGE OF aufnr.

  FREE:  gt_zpmr0011,  gt_zpmr0002.

  SELECT *
   FROM zpmr0002
   INTO CORRESPONDING FIELDS OF TABLE gt_zpmr0002
   WHERE aprovador  EQ i_user
      OR usua_subst EQ i_user. "Anderson Oenning
*  if sy-subrc ne 0.
  SELECT *
    FROM zpmr0011
    INTO CORRESPONDING FIELDS OF TABLE gt_zpmr0011
    WHERE aprovador  EQ i_user
       OR usua_subst EQ i_user. "Anderson Oenning

  IF sy-subrc EQ 0.

    "FF #185560 - inicio
    " Flags de controle
    DATA(lv_has_0002) = xsdbool( lines( gt_zpmr0002 ) > 0 ).
    DATA(lv_has_0011) = xsdbool( lines( gt_zpmr0011 ) > 0 ).
    "FF #185560 - fim

    DATA: gt_zpmr0002_aux TYPE TABLE OF zpmr0002 WITH HEADER LINE.

    MOVE-CORRESPONDING gt_zpmr0011[] TO gt_zpmr0002_aux[]. "US # - MMSILVA - 08.05.2025
    APPEND LINES OF gt_zpmr0002_aux[] TO gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025

  ENDIF.

  DELETE ADJACENT DUPLICATES FROM gt_zpmr0002[]. "US # - MMSILVA - 08.05.2025
*  endif.

  LOOP AT gt_zpmr0002 ASSIGNING FIELD-SYMBOL(<wa_estrategia>).
    IF <wa_estrategia>-aprovador IS NOT INITIAL
      AND <wa_estrategia>-usua_subst IS NOT INITIAL
      AND <wa_estrategia>-data_lim >= sy-datum.
      <wa_estrategia>-aprovador = <wa_estrategia>-usua_subst.
    ENDIF.
  ENDLOOP.

  DELETE gt_zpmr0002 WHERE aprovador NE i_user.
  CHECK gt_zpmr0002[] IS NOT INITIAL.

* Range de centro
  r_werks = VALUE #( FOR ls1 IN gt_zpmr0002 ( sign = 'I' option = 'EQ' low = ls1-centro_desp ) ).

  SELECT *
    FROM ztparam
    INTO TABLE @DATA(_parameters)
   WHERE param = 'TP_ORDEM'
     AND const IN @r_werks.

* Range de tipo de Ordens
  order_types = VALUE #( FOR ls2 IN _parameters ( sign = 'I' option = 'EQ' low = ls2-zval ) ).

  SORT order_types BY low.
  DELETE ADJACENT DUPLICATES FROM order_types COMPARING low.


  "FF #185560 - inicio
  IF lv_has_0011 = abap_true AND lv_has_0002 = abap_false.
    "=== Caso 1: Usuário existe apenas na ZPMR0011
    "Buscar por centro de custo (KOSTL) + centro (WERKS)


    SELECT DISTINCT a~mandt a~werks a~erdat h~equnr e~eqktx a~aufnr a~objnr
                 a~ktext a~user4 a~erfzeit i~counter h~iwerk i~pmsog
   INTO CORRESPONDING FIELDS OF TABLE gt_ordens_aux
   FROM aufk AS a
   INNER JOIN ihsg  AS i ON i~objnr = a~objnr
   INNER JOIN afih  AS h ON h~aufnr = a~aufnr
   LEFT  JOIN eqkt  AS e ON e~equnr = h~equnr
   FOR ALL ENTRIES IN gt_zpmr0011
   WHERE ( a~phas0 = abap_true AND a~phas1 = abap_false )
     AND a~auart IN order_types
     AND a~werks = gt_zpmr0011-centro_desp
     AND a~kostl = gt_zpmr0011-kostl
     AND i~lvorm = abap_false.

  ELSE.
    "=== Caso 2: Usuário existe na ZPMR0002 (com ou sem ZPMR0011)
    "Buscar apenas por centro (WERKS)

    "FF #185560 - fim


* Seleção das Ordens para aprovar
    SELECT DISTINCT a~mandt werks a~erdat h~equnr e~eqktx a~aufnr a~objnr a~ktext a~user4 a~erfzeit i~counter h~iwerk i~pmsog
      INTO CORRESPONDING FIELDS OF TABLE gt_ordens_aux
      FROM aufk AS a
      INNER JOIN ihsg  AS i ON i~objnr EQ a~objnr
      INNER JOIN afih  AS h ON h~aufnr EQ a~aufnr
      LEFT  JOIN eqkt  AS e ON e~equnr EQ h~equnr
      FOR ALL ENTRIES IN gt_zpmr0002
      WHERE ( a~phas0 EQ abap_true  AND
              a~phas1 EQ abap_false )
        AND a~auart IN order_types
        AND a~werks EQ gt_zpmr0002-centro_desp
        AND i~lvorm EQ abap_false.

  ENDIF.

  CHECK gt_ordens_aux[] IS NOT INITIAL.
  SORT gt_ordens_aux[] BY aufnr counter.

  gt_ordens = gt_ordens_aux[].
  SORT gt_ordens BY objnr counter.
  DELETE ADJACENT DUPLICATES FROM gt_ordens COMPARING objnr counter.

  IF gt_ordens IS NOT INITIAL.

* get nos dados que já estão aprovados
    SELECT *
      FROM ihgns
      INTO TABLE @DATA(lt_ihgns)
      FOR ALL ENTRIES IN @gt_ordens
      WHERE objnr   EQ @gt_ordens-objnr
       AND  counter EQ @gt_ordens-counter
       AND  geniakt EQ @abap_false.
    IF sy-subrc IS INITIAL.
      SORT lt_ihgns BY objnr counter ASCENDING gendatum DESCENDING gentime DESCENDING.
    ENDIF.

* get nos dados com o Nivel de cada Ordem
    FREE it_ihsg.
    SELECT DISTINCT a~objnr,a~counter,a~ernam,a~pmsog,b~nivel,b~aprovador,b~usua_subst,b~data_lim,b~centro_desp
      FROM ihsg AS a
      LEFT JOIN zpmr0002 AS b ON b~permit EQ a~pmsog
        INTO TABLE @DATA(lt_ihsg)
          FOR ALL ENTRIES IN @gt_ordens
            WHERE objnr   EQ @gt_ordens-objnr
             AND  counter EQ @gt_ordens-counter
             AND  b~centro_desp EQ @gt_ordens-werks.

*    * get nos dados com o Nivel de cada Ordem
    FREE it_ihsg.
    SELECT DISTINCT a~objnr,a~counter,a~ernam,a~pmsog,b~nivel,b~aprovador,b~usua_subst,b~data_lim,b~centro_desp
      FROM ihsg AS a
      LEFT JOIN zpmr0011 AS b ON b~permit EQ a~pmsog
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_ihsg
          FOR ALL ENTRIES IN @gt_ordens
            WHERE objnr   EQ @gt_ordens-objnr
             AND  counter EQ @gt_ordens-counter
             AND  b~centro_desp EQ @gt_ordens-werks.

    IF sy-subrc IS INITIAL.
      SORT lt_ihsg BY objnr nivel.
    ENDIF.

  ENDIF.

  LOOP AT gt_ordens_aux.
    IF objnr_aux NE gt_ordens_aux-objnr.
      objnr_aux = gt_ordens_aux-objnr.
    ELSE.
      CONTINUE.
    ENDIF.

    DATA(gt_ihgns) = lt_ihgns.
    MOVE-CORRESPONDING lt_ihsg TO it_ihsg.

    FREE gt_ordens.
    gt_ordens = VALUE #( FOR ls IN gt_ordens_aux WHERE ( objnr EQ gt_ordens_aux-objnr ) ( ls ) ).


    DELETE gt_ihgns WHERE objnr   NE gt_ordens_aux-objnr.
*                          counter NE gt_ordens_aux-counter.

** get nos dados que já estão aprovados
*    SELECT *
*      FROM ihgns
*      INTO TABLE @DATA(gt_ihgns)
*      FOR ALL ENTRIES IN @gt_ordens
*      WHERE objnr   EQ @gt_ordens-objnr
*       AND  counter EQ @gt_ordens-counter
*       AND  geniakt EQ @abap_false.
*
*    SORT gt_ihgns BY objnr counter ASCENDING gendatum DESCENDING gentime DESCENDING.

    DELETE it_ihsg WHERE objnr       NE gt_ordens_aux-objnr OR
                         centro_desp NE gt_ordens_aux-werks.

** get nos dados com o Nivel de cada Ordem
*    FREE it_ihsg.
*    SELECT DISTINCT a~objnr a~counter a~ernam a~pmsog b~nivel b~aprovador b~usua_subst b~data_lim
*      FROM ihsg AS a
*      INNER JOIN zpmr0002 AS b ON b~permit EQ a~pmsog
*        INTO CORRESPONDING FIELDS OF TABLE it_ihsg
*          FOR ALL ENTRIES IN gt_ordens
*            WHERE objnr   EQ gt_ordens-objnr
*             AND  counter EQ gt_ordens-counter
*             AND  b~centro_desp EQ gt_ordens-werks.
*
*    SORT it_ihsg BY objnr nivel.

* Desmarca o Check da Ordem a ser aprovada
    LOOP AT it_ihsg ASSIGNING FIELD-SYMBOL(<wa2>) WHERE check IS INITIAL.
      IF line_exists( gt_ihgns[ objnr = <wa2>-objnr counter = <wa2>-counter ] ).
        <wa2>-check = abap_true.
      ENDIF.

      IF <wa2>-usua_subst IS NOT INITIAL AND <wa2>-data_lim >= sy-datum .
        <wa2>-aprovador = <wa2>-usua_subst. "Anderson Oenning
      ENDIF.
    ENDLOOP.

* deleta todas as Ordens com Check e diferente do aprovador selecionado
    DELETE it_ihsg WHERE check IS NOT INITIAL.

    SORT it_ihsg BY objnr nivel.
    CLEAR: objnr_, nivel_.
    LOOP AT it_ihsg ASSIGNING <wa2>.
      IF objnr_ NE <wa2>-objnr AND nivel_ NE <wa2>-nivel.
        objnr_ = <wa2>-objnr.
        nivel_ = <wa2>-nivel.
        CONTINUE.
      ENDIF.

      IF objnr_ EQ <wa2>-objnr AND nivel_ EQ <wa2>-nivel.
        CONTINUE.
      ENDIF.
      <wa2>-check = abap_true.
    ENDLOOP.

* deleta todas as Ordens com Check
    DELETE it_ihsg WHERE check IS NOT INITIAL.
    DELETE it_ihsg WHERE aprovador NE i_user .

* adiciona na saida todas as Ordens que estão no IHSG
    it_ordens = VALUE #( FOR ls3 IN gt_ordens
                         FOR ls4 IN  it_ihsg WHERE ( objnr = ls3-objnr AND counter = ls3-counter )
                        ( CORRESPONDING #( ls3 ) )
                      ).

    LOOP AT it_ordens INTO DATA(ls_ordens).
      MOVE-CORRESPONDING ls_ordens TO lw_ordens.
      APPEND lw_ordens TO t_ordens[].
    ENDLOOP.
  ENDLOOP.

*  APPEND LINES OF T_ORDENS TO T_ORDENS_AUX.
*  FREE T_ORDENS.
*
*  LOOP AT GT_ZPMR0002 INTO DATA(W_0002).
*    LOOP AT T_ORDENS_AUX ASSIGNING FIELD-SYMBOL(<ORDEM>).
*      IF <ORDEM>-USER4 BETWEEN W_0002-VALOR_DE AND W_0002-VALOR_ATE.
*        CLEAR <ORDEM>-WERT2.
*        APPEND <ORDEM> TO T_ORDENS.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.

  "//Verifica se há Ordens Reprovadas
  SELECT  *
    FROM zpmr0006
    INTO TABLE @DATA(historico)
    FOR ALL ENTRIES IN @t_ordens
   WHERE aufnr  = @t_ordens-aufnr
     AND status = 'R'.

  IF sy-subrc IS INITIAL.
    LOOP AT historico  INTO DATA(wa).
      LOOP AT t_ordens ASSIGNING FIELD-SYMBOL(<wa1>) WHERE aufnr EQ wa-aufnr.
        IF ( wa-vlr_estimado EQ <wa1>-user4 ).
          <wa1>-status = 'R'.
        ELSE.
          <wa1>-status = 'P'.
          DELETE FROM zpmr0006 WHERE aufnr = <wa1>-aufnr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.


  IF t_ordens[] IS NOT INITIAL."167440 CS2025000208 ZPM0013 POR CENTRO DE CUSTO PSA

    TYPES: BEGIN OF ty_ordens2.
             INCLUDE STRUCTURE t_ordens.
    TYPES:   kostl     TYPE kostl,
             aprovador TYPE bname,
           END OF ty_ordens2.

    DATA: t_ordens2 TYPE STANDARD TABLE OF ty_ordens2 INITIAL SIZE 0,
          w_ordens2 TYPE ty_ordens2.

    FREE: rg_aufnr_e[], rg_aufnr_d[], r_werks[].

    LOOP AT t_ordens[] ASSIGNING FIELD-SYMBOL(<fs_ordens>).
      MOVE-CORRESPONDING <fs_ordens> TO w_ordens2.
      SELECT SINGLE kostl FROM aufk WHERE aufnr = @<fs_ordens>-aufnr INTO @w_ordens2-kostl.
      IF sy-subrc = 0.
        SELECT * FROM zpmr0011
          WHERE kostl = @w_ordens2-kostl
         AND aprovador  EQ @i_user
         AND nivel      <= @<fs_ordens>-counter
          OR usua_subst EQ @i_user
              INTO TABLE @DATA(it_zpmr0011).
        IF sy-subrc EQ 0.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) TO rg_aufnr_e[].
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-werks ) TO r_werks[].
        ELSE.
*
          "Verifica aprovação.
*          read table gt_zpmr0002 into data(wa_zpmr0002) with key centro_desp = <fs_ordens>-werks.
*          if sy-subrc eq 0.
*            append value #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) to rg_aufnr_e[].
*          endif.
**
*            w_ordens2-aprovador = i_user.

*         Verifica se para esse centro de custo existe aprovador.
          SELECT SINGLE * FROM zpmr0011
            INTO @DATA(ws_zpmr0011)
          WHERE kostl EQ @w_ordens2-kostl
            AND nivel <= @<fs_ordens>-counter.
          IF sy-subrc EQ 0.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) TO rg_aufnr_d[].
          ENDIF.
        ENDIF.
      ENDIF.

*     US # - MMSILVA - 08.05.2025 - Inicio
      SELECT SINGLE * FROM zpmr0002 WHERE ( aprovador = @i_user OR usua_subst = @i_user ) AND centro_desp = @<fs_ordens>-werks INTO @DATA(wa_zpmr0002).
      IF sy-subrc EQ 0.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ordens>-aufnr ) TO rg_aufnr_e[].
      ENDIF.
*     US # - MMSILVA - 08.05.2025 - Fim

*        append w_ordens2 to t_ordens2.
*        clear:  w_ordens2.
*      endif.
    ENDLOOP.



*    if t_ordens2 is not initial.
*      loop at t_ordens2 assigning field-symbol(<fs_ordens2>) where aprovador <> sy-uname.
*        delete t_ordens where aufnr = <fs_ordens2>-aufnr.
*      endloop.
*    endif.

    IF rg_aufnr_e[] IS NOT INITIAL.
      DELETE t_ordens WHERE aufnr NOT IN rg_aufnr_e[] AND werks IN r_werks[].
    ENDIF.

    IF rg_aufnr_d[] IS NOT INITIAL.
      DELETE t_ordens WHERE aufnr IN rg_aufnr_d[].
    ENDIF.

    FREE: t_ordens2.

    DELETE ADJACENT DUPLICATES FROM t_ordens[]. "US # - MMSILVA - 08.05.2025

  ENDIF.

ENDFUNCTION.
