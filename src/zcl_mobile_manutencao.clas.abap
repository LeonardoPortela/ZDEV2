class ZCL_MOBILE_MANUTENCAO definition
  public
  final
  create public .

public section.

  interfaces ZIF_MOBILE_MANUTENCAO .

  class-methods ME_GET_D_NOTA
    importing
      !FILIAL type IWERK
      value(DATA) type RSIS_T_RANGE
    returning
      value(D_NOTA) type ZT_ZEPM_D_NOT .
  class-methods ME_SAVE_D_NOTA
    importing
      !D_NOTA type ZT_ZEPM_D_NOT
    returning
      value(R_RETURN) type STRING .
  class-methods ME_SET_EQPTO .
  class-methods ME_GET_EQPTO
    returning
      value(VEICULOS) type ZT_ZTPM_M_VEIC_MOBILE .
  class-methods ME_SAVE_EQPTO
    importing
      !T_VEICULOS type ZT_ZTPM_M_VEIC_MOBILE
    returning
      value(R_RETURN) type STRING .
  class-methods ME_SET_LOCAL .
  class-methods ME_GET_LOCAL
    returning
      value(T_LOCAL) type ZT_ZTPM_M_LOCAL_MOBILE .
  class-methods ME_SAVE_LOCAL
    importing
      value(T_LOCAL) type ZT_ZTPM_M_LOCAL_MOBILE
    returning
      value(R_RETURN) type STRING .
  class-methods ME_SET_ORDEM
    exporting
      value(T_ORDEM) type ZT_ZTPM_D_M_ORDEM .
  class-methods ME_GET_ORDEM
    importing
      !DATA type RSIS_T_RANGE
    returning
      value(T_ORDEM) type ZT_ZTPM_D_M_ORDEM .
  class-methods ME_SAVE_ORDEM
    importing
      value(T_ORDEM) type ZT_ZTPM_D_M_ORDEM
    returning
      value(R_RETURN) type STRING .
  class-methods ME_SET_USER .
  class-methods ME_GET_USER
    returning
      value(T_USER) type ZT_ZTPM_D_M_USUARIO .
  class-methods ME_SAVE_USER
    importing
      value(T_USER) type ZT_ZTPM_D_M_USUARIO
    returning
      value(R_RETURN) type STRING .
  class-methods ME_SET_OPER_ORDEM
    importing
      !I_ORDEM type ZT_ZTPM_D_M_ORDEM .
  class-methods ME_GET_OPER_ORDEM
    importing
      value(I_ORDEM) type ZT_ZTPM_D_M_ORDEM
    returning
      value(T_OPERACAO) type ZT_ZTPM_D_M_OPERACAO .
  class-methods ME_SAVE_OPER_ORDEM
    importing
      value(T_OPERACAO) type ZT_ZTPM_D_M_OPERACAO
    returning
      value(R_RETURN) type STRING .
  class-methods ME_REGISTRA_LOG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MOBILE_MANUTENCAO IMPLEMENTATION.


  METHOD me_get_d_nota.

    DATA: r_objid TYPE RANGE OF objid.

    TYPES:
      BEGIN OF ty_equipment,
        iwerk TYPE equz-iwerk,
        equnr TYPE equz-equnr,
        eqart TYPE equi-eqart,
      END OF ty_equipment,


      BEGIN OF ty_catalog,
        codigo TYPE qpcd-code,
        texto  TYPE qpct-kurztext,
        grupo  TYPE qpcd-codegruppe,
      END OF ty_catalog,

      BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        txt04 TYPE tj02t-txt04,
        inact	TYPE jest-inact,
      END OF ty_jest.


    DATA: gt_tq80 TYPE TABLE OF tq80.
    DATA: gt_jest TYPE TABLE OF ty_jest.
    DATA: w_jest TYPE ty_jest.
    DATA: w_qmur TYPE qmur.
    DATA: gt_catal TYPE TABLE OF zepm_d_nota.
    DATA: wa_catal TYPE ztpm_d_n_catal,
          l_name   TYPE thead-tdname.

    DATA: t_d_nota      TYPE TABLE OF zepm_d_not,
          t_sort_d_nota TYPE SORTED TABLE OF zepm_d_not WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_tqscr       TYPE TABLE OF tqscr,
          t_sort_tqscr  TYPE SORTED TABLE OF tqscr WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_qmfe        TYPE TABLE OF qmfe,
          t_sort_qmfe   TYPE SORTED TABLE OF qmfe WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_qmur        TYPE TABLE OF qmur,
          t_sort_qmur   TYPE SORTED TABLE OF qmur WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_viqmma      TYPE TABLE OF viqmma,
          t_sort_viqmma TYPE SORTED TABLE OF viqmma WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_texto       TYPE TABLE OF tline,
          w_texto       TYPE tline.

    DATA: t_notlongtxt TYPE TABLE OF bapi2080_notfulltxte,
          it_return    TYPE TABLE OF bapiret2.

    READ TABLE data INTO DATA(w_date) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(dt_inic) = w_date-low.
      DATA(dt_fim) = w_date-high.
    ENDIF.


    SELECT *
     FROM viqmel AS a
     INNER JOIN tq80 AS b ON b~qmart = a~qmart
     INTO CORRESPONDING FIELDS OF TABLE t_d_nota
      WHERE a~qmdat IN data
*     WHERE a~qmdat BETWEEN dt_inic AND dt_fim
       AND b~qmtyp EQ '01'
*       AND a~qmart IN ( 'Y1', 'Y2', 'Y3', 'MI', 'M2', 'M3', 'P1', 'P2', 'P3' )
       AND ( EXISTS ( SELECT * FROM jest
                       WHERE objnr EQ a~objnr
                         AND inact NE abap_true
                         AND stat  = 'I0070' ) ).

    CHECK t_d_nota IS NOT INITIAL.
    SORT t_d_nota[] BY qmnum ASCENDING.


    SELECT *
    FROM qmfe
    INTO TABLE t_qmfe
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmnum EQ t_d_nota-qmnum.
    SORT t_qmfe BY otkat otgrp oteil fekat fegrp fecod.
    DELETE t_qmfe
    WHERE otkat EQ space
    AND otgrp EQ space
    AND oteil EQ space
    AND fekat EQ space
    AND fegrp EQ space
    AND  fecod EQ space.
    SORT t_qmfe BY qmnum.

    IF t_qmfe IS NOT INITIAL.
      SORT t_qmfe[] BY qmnum fenum ASCENDING.
      SELECT *
      FROM qmur
      INTO TABLE t_qmur
        FOR ALL ENTRIES IN t_qmfe
        WHERE qmnum EQ t_qmfe-qmnum
         AND  fenum EQ t_qmfe-fenum.
      SORT t_qmur[] BY urkat urgrp urcod ASCENDING.
      DELETE t_qmur[] WHERE urkat EQ space AND urgrp EQ space AND urcod EQ space.
      SORT t_qmur[] BY qmnum fenum ASCENDING.
    ENDIF.

    SELECT *
    FROM viqmma
    INTO TABLE t_viqmma
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmnum EQ t_d_nota-qmnum.
    SORT t_viqmma[] BY mnkat mngrp  mncod ASCENDING.
    DELETE t_viqmma[] WHERE mnkat EQ space AND mngrp EQ space AND mncod EQ space.
    SORT t_viqmma[] BY qmnum ASCENDING.

    SELECT *
    FROM tqscr
    INTO TABLE t_tqscr
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmart EQ t_d_nota-qmart
        AND qmtyp EQ '01'
        AND tabcd EQ '10\TAB00'.
    SORT t_tqscr[] BY sub03 ASCENDING.
    DELETE t_tqscr[] WHERE sub03 NE '035'.
    SORT t_tqscr[] BY qmart qmtyp tabcd ASCENDING.

    t_sort_d_nota[] = t_d_nota[].
    t_sort_tqscr[]  = t_tqscr[].
    t_sort_qmur[]   = t_qmur[].
    t_sort_qmfe[]   = t_qmfe[].
    t_sort_viqmma[] = t_viqmma[].

    LOOP AT t_d_nota ASSIGNING FIELD-SYMBOL(<_nota>).

      FREE: t_texto.

      <_nota>-txt04 = 'LIB'.
      <_nota>-marc_status = abap_true.
      <_nota>-istat = '1'.

      <_nota>-iwerk = <_nota>-swerk.
*      <_nota>-arjty = 'A'.
*      <_nota>-arjid = CONV #( <_nota>-arbpl ).
      CLEAR t_notlongtxt.

*-IR054443 - 14.04.2021 - JT - inicio
      l_name = <_nota>-qmnum.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'LTXT'
          language                = sy-langu
          name                    = l_name
          object                  = 'QMEL'
        TABLES
          lines                   = t_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT t_texto ASSIGNING FIELD-SYMBOL(<_text_l>).
        IF <_nota>-txtnt IS INITIAL.
          <_nota>-txtnt =  | ->{ <_text_l>-tdline }| .
        ELSE.
          <_nota>-txtnt = |{ <_nota>-txtnt } ->{ <_text_l>-tdline }|.
        ENDIF.
      ENDLOOP.

*     CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
*       EXPORTING
*         number     = <_nota>-qmnum
*       TABLES
*         notlongtxt = t_notlongtxt
*         return     = it_return.

*     LOOP AT t_notlongtxt ASSIGNING FIELD-SYMBOL(<_text>).
*       IF <_nota>-txtnt IS INITIAL.
*         <_nota>-txtnt =  | ->{ <_text>-text_line }| .
*       ELSE.
*         <_nota>-txtnt = |{ <_nota>-txtnt } ->{ <_text>-text_line }|.
*       ENDIF.
*     ENDLOOP.
*-IR054443 - 14.04.2021 - JT - fim

      READ TABLE t_tqscr ASSIGNING FIELD-SYMBOL(<_tqscr>) WITH KEY qmart = <_nota>-qmart
                                                                        qmtyp = '01'
                                                                        tabcd = '10\TAB00' BINARY SEARCH.


      IF sy-subrc EQ 0.
        <_nota>-avaria = abap_true.

        LOOP AT t_sort_qmfe ASSIGNING FIELD-SYMBOL(<w_qmfe>) WHERE qmnum = <_nota>-qmnum.
          wa_catal-otkat = <w_qmfe>-otkat.
          wa_catal-otgrp = <w_qmfe>-otgrp.
          wa_catal-oteil = <w_qmfe>-oteil.
          wa_catal-fekat = <w_qmfe>-fekat.
          wa_catal-fegrp = <w_qmfe>-fegrp.
          wa_catal-fecod = <w_qmfe>-fecod.

          READ TABLE t_qmur INTO w_qmur WITH KEY qmnum = <w_qmfe>-qmnum
                                                 fenum = <w_qmfe>-fenum BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_catal-urkat  = w_qmur-urkat.
            wa_catal-urgrp  = w_qmur-urgrp.
            wa_catal-urcod  = w_qmur-urcod.
          ENDIF.

          IF wa_catal IS NOT INITIAL.
            APPEND wa_catal TO <_nota>-part_objnr.
            CLEAR: wa_catal, w_qmur.
          ENDIF.
        ENDLOOP.

      ELSE.
        <_nota>-avaria = ''.
        LOOP AT t_sort_viqmma ASSIGNING FIELD-SYMBOL(<w_qmma>) WHERE qmnum = <_nota>-qmnum.
          wa_catal-mnkat =  <w_qmma>-mnkat.
          wa_catal-mngrp =  <w_qmma>-mngrp.
          wa_catal-mncod =  <w_qmma>-mncod.

          IF wa_catal IS NOT INITIAL.
            APPEND wa_catal TO <_nota>-part_objnr.
            CLEAR wa_catal.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING t_d_nota TO d_nota.
  ENDMETHOD.


  METHOD me_get_eqpto.
    TYPES:
      BEGIN OF ty_equipment,
        iwerk TYPE equz-iwerk,
        equnr TYPE equz-equnr,
        eqart TYPE equi-eqart,
      END OF ty_equipment.

    DATA:tb_veiculo   TYPE TABLE OF ztpm_m_veic_mobile,
         tb_equipment TYPE TABLE OF ty_equipment,
         lv_line      TYPE bsvx-sttxt.

    FREE: tb_equipment.


    SELECT DISTINCT b~iwerk a~equnr a~eqart
    INTO CORRESPONDING FIELDS OF TABLE tb_equipment
    FROM equi AS a
   INNER JOIN equz AS b ON b~equnr = a~equnr
   WHERE b~datbi EQ '99991231'
     AND a~eqtyp IN ( 'A', 'E', 'V' )
     AND NOT EXISTS ( SELECT *
                       FROM jest
                       WHERE stat IN ( 'I0076', 'I0320' )
                        AND inact EQ abap_false
                        AND objnr EQ a~objnr
                     ).


    SORT tb_equipment ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM tb_equipment.

    IF tb_equipment IS NOT INITIAL.
      SELECT a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx
         FROM equz AS a
         INNER JOIN equi AS b ON b~equnr EQ a~equnr
         INNER JOIN eqkt AS c ON c~equnr EQ b~equnr
         INNER JOIN iloa AS d ON d~iloan EQ a~iloan
         INNER JOIN iflotx AS e ON e~tplnr EQ d~tplnr
         INTO CORRESPONDING FIELDS OF TABLE veiculos
        FOR ALL ENTRIES IN tb_equipment
           WHERE a~equnr EQ tb_equipment-equnr
            AND  a~datbi EQ '99991231'
            AND  c~spras EQ sy-langu.
*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
      IF sy-subrc IS INITIAL.
        LOOP AT veiculos ASSIGNING FIELD-SYMBOL(<fs_veiculos>).
          IF <fs_veiculos>-objnr IS NOT INITIAL.

            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                client           = sy-mandt
                objnr            = <fs_veiculos>-objnr
                spras            = sy-langu
              IMPORTING
                line             = lv_line
              EXCEPTIONS
                object_not_found = 1
                OTHERS           = 2.
            IF sy-subrc = 0.
              IF lv_line EQ 'INAT' OR lv_line EQ 'MREL'.
                <fs_veiculos>-istat = '0'.
              ELSE.
                <fs_veiculos>-istat = '1'.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
    ENDIF.
  ENDMETHOD.


  METHOD me_get_local.

    SELECT A~ILOAN A~TPLNR A~SWERK A~PLTXT
      FROM iflo AS a
      INNER JOIN iflotx AS b ON b~tplnr EQ a~tplnr
        INTO CORRESPONDING FIELDS OF TABLE t_local
        WHERE ( EXISTS ( SELECT * FROM jest
                     WHERE objnr EQ a~objnr
                       AND inact NE abap_true
                       AND stat  EQ 'I0098' ) ).


  ENDMETHOD.


  METHOD me_get_oper_ordem.
    DATA: gt_conf TYPE TABLE OF afru.

    CHECK i_ordem IS NOT INITIAL.

    SELECT *
     FROM afko AS a
     INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
     INNER JOIN afvv AS c ON c~aufpl EQ b~aufpl AND c~aplzl EQ b~aplzl
     INTO CORRESPONDING FIELDS OF TABLE t_operacao
       FOR ALL ENTRIES IN i_ordem
       WHERE a~aufnr EQ i_ordem-aufnr
        AND b~loekz EQ abap_false.
    SORT t_operacao ASCENDING BY aufnr vornr.

    FREE gt_conf.
    SELECT *
    FROM afru
    INTO TABLE gt_conf
      FOR ALL ENTRIES IN t_operacao
      WHERE aufnr EQ t_operacao-aufnr
        AND vornr EQ t_operacao-vornr.

    SORT gt_conf ASCENDING BY aufnr vornr aueru.
    DELETE gt_conf WHERE aueru NE abap_true.

    LOOP AT t_operacao ASSIGNING FIELD-SYMBOL(<_operacao>).
      IF <_operacao>-steus EQ 'PM01'.
        <_operacao>-steus = '1'.
      ELSE.
        <_operacao>-steus = '0'.
      ENDIF.

      SELECT SINGLE *
      FROM crhd
      INTO @DATA(s_crhd)
        WHERE objid EQ @<_operacao>-arbid.
      IF sy-subrc EQ 0.
        <_operacao>-objty = s_crhd-objty.
      ENDIF.
      CLEAR s_crhd.

      LOOP AT gt_conf INTO DATA(_conf) WHERE aufnr = <_operacao>-aufnr
                                        AND  vornr = <_operacao>-vornr.

        CHECK sy-subrc EQ 0.

        IF _conf-aueru EQ abap_true.
          <_operacao>-confnl = _conf-aueru.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD me_get_ordem.

    TYPES:
      BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        txt04 TYPE tj02t-txt04,
        inact	TYPE jest-inact,
      END OF ty_jest.

    DATA: gt_jest TYPE TABLE OF ty_jest.
    DATA: w_jest TYPE ty_jest.
    DATA: r_stat    TYPE rsis_t_range,
          t_t356_t  TYPE TABLE OF t356_t,
          t_iflo    TYPE TABLE OF iflo,
          t_t353i_t TYPE TABLE OF t353i_t,
          t_v_auart TYPE TABLE OF v_auart,
          t_eqkt    TYPE TABLE OF eqkt,
          t_crhd    TYPE TABLE OF crhd,
          t_equip   TYPE TABLE OF zpmt0022.

    DATA: i_data_de TYPE sy-datum.
    DATA: i_data_ate TYPE sy-datum.
    DATA: _data TYPE p DECIMALS 2.

*    DATA: R_STAT LIKE RANGE OF JEST-STAT.
*    DATA line_range LIKE LINE OF status_range.
    DATA(aber) = 'I0001'.
    DATA(lib) =  'I0002'.


    APPEND VALUE #( sign = 'I' option = 'EQ' low = lib  high = ' ' )  TO r_stat.

    "Pegando periodo de 360 dias.
    _data = 900. "Periodo de 360 dias.
    i_data_de = sy-datum - _data.
    i_data_ate = sy-datum.


    SELECT *
    FROM viaufkst
    INTO CORRESPONDING FIELDS OF TABLE t_ordem
    WHERE erdat IN data
      AND auart NOT IN ( 'ZPM3', 'ZPM5', 'ZPM6' ,'ID06', 'HE06' )
      AND autyp EQ '30'
      AND ( EXISTS ( SELECT * FROM jest
                     WHERE objnr EQ viaufkst~objnr
                       AND inact NE abap_true
                       AND stat  EQ 'I0002' ) ).

    CHECK t_ordem IS NOT INITIAL.
    SORT t_ordem BY aufnr ASCENDING.

*Coletando descrição da prioridade.
    SELECT *
    FROM t356_t
    INTO TABLE t_t356_t
    FOR ALL ENTRIES IN t_ordem
      WHERE priok EQ t_ordem-priok
        AND artpr EQ 'PM'
        AND spras EQ 'P'.
    SORT t_t356_t BY priok ASCENDING.

*   Coletando descrição da local de instalação.
    SELECT *
   FROM iflo
   INTO TABLE t_iflo
      FOR ALL ENTRIES IN t_ordem
     WHERE tplnr EQ t_ordem-tplnr.
    SORT t_iflo BY tplnr ASCENDING.

*   Coletando descrição da atividade.
    SELECT *
    FROM t353i_t
    INTO TABLE t_t353i_t
      FOR ALL ENTRIES IN t_ordem
      WHERE ilart EQ t_ordem-ilart
      AND   spras EQ 'P'.
    SORT t_t353i_t BY ilart ASCENDING.


*   Coletando descrição do tipo de ordem.
    SELECT *
    FROM v_auart
    INTO TABLE t_v_auart
      FOR ALL ENTRIES IN t_ordem
    WHERE auart EQ t_ordem-auart
    AND  spras EQ 'P'.
    SORT t_v_auart BY auart ASCENDING.

*   Coletando descrição do equipamento.
    SELECT *
    FROM eqkt
    INTO TABLE t_eqkt
      FOR ALL ENTRIES IN t_ordem
      WHERE equnr EQ t_ordem-equnr.
    SORT t_eqkt BY equnr ASCENDING.

* Coletando centro de trabalho responsável.
    SELECT *
    FROM crhd
    INTO TABLE t_crhd
      FOR ALL ENTRIES IN t_ordem
      WHERE objid EQ t_ordem-gewrk.
    SORT t_crhd BY objid ASCENDING.

    IF t_ordem IS NOT INITIAL.
      SELECT *
      FROM zpmt0022
      INTO TABLE t_equip
      FOR ALL ENTRIES IN t_ordem
      WHERE aufnr EQ t_ordem-aufnr.
      SORT t_equip BY aufnr ASCENDING.

    ENDIF.

    LOOP AT t_ordem ASSIGNING FIELD-SYMBOL(<ls_ordem>).
      READ TABLE  t_t356_t INTO DATA(_t356_t) WITH KEY priok = <ls_ordem>-priok.
      IF sy-subrc EQ 0.
        <ls_ordem>-artpr  = _t356_t-artpr.
        <ls_ordem>-priokx = _t356_t-priokx.
      ENDIF.

      READ TABLE  t_iflo INTO DATA(_iflo) WITH KEY tplnr = <ls_ordem>-tplnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-pltxt = _iflo-pltxt.
      ENDIF.

      READ TABLE  t_t353i_t INTO DATA(_t353i_t) WITH KEY ilart = <ls_ordem>-ilart BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-ilatx = _t353i_t-ilatx.
      ENDIF.

      READ TABLE  t_v_auart INTO DATA(_v_auart) WITH KEY auart = <ls_ordem>-auart BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-txt = _v_auart-txt.
      ENDIF.

      IF <ls_ordem>-equnr IS NOT INITIAL.
        READ TABLE  t_eqkt INTO DATA(_eqkt) WITH KEY equnr = <ls_ordem>-equnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_ordem>-eqktx = _eqkt-eqktx.
        ENDIF.
      ENDIF.

      READ TABLE  t_crhd INTO DATA(_crhd) WITH KEY objid = <ls_ordem>-gewrk BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-cr_objty = _crhd-objty.
        <ls_ordem>-arbpl    = _crhd-arbpl.
      ENDIF.

      READ TABLE t_equip INTO DATA(_eqp) WITH KEY aufnr = <ls_ordem>-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-idequipe = _eqp-idequipe.
        <ls_ordem>-dsequipe = _eqp-dsequipe.
      ENDIF.

      <ls_ordem>-txt04 = 'LIB'.
      <ls_ordem>-marc_status = abap_true.
      <ls_ordem>-istat = '1'.
      CLEAR: _t356_t, _iflo, _t353i_t, _v_auart, _eqkt, _crhd.
    ENDLOOP.

  ENDMETHOD.


  METHOD me_get_user.

*    CALL FUNCTION 'RP_GET_FIRE_DATE'.
    DATA: gt_empregado TYPE TABLE OF ztpm_d_m_empreg.
    DATA: gw_empregado TYPE TABLE OF ztpm_d_m_empreg.
    DATA: gt_user TYPE TABLE OF ztpm_d_usuario.
    DATA: gw_usuario TYPE ztpm_d_usuario.

    DATA: gw_perf TYPE TABLE OF zpmt0012.
    DATA: gs_perf TYPE ztpm_d_m_perfil.
    DATA: gs_ctrab TYPE ztpm_d_m_centrab.
    DATA: gt_usuario TYPE TABLE OF ztpm_d_m_usuario.
    DATA: t_usuario TYPE TABLE OF pa0002.
    DATA: t_depart TYPE TABLE OF zhcmt0007.
    DATA: w_usuario TYPE pad_cname.

*Consultado cadastro de empregado.
    CLEAR gt_usuario.
    SELECT *
    FROM pa0465
    INTO CORRESPONDING FIELDS OF TABLE gt_usuario
      WHERE tpdoc EQ '0001'.

    CHECK gt_usuario IS NOT INITIAL.

*Consultado cadastro de empregado ativos.
    SELECT *
    FROM pa0000
    INTO TABLE @DATA(s_pa0000)
      FOR ALL ENTRIES IN @gt_usuario
      WHERE pernr EQ @gt_usuario-pernr
       AND  stat2  EQ '0'.

    SELECT *
     FROM pa0002
     INTO CORRESPONDING FIELDS OF TABLE t_usuario
      FOR ALL ENTRIES IN gt_usuario
       WHERE pernr EQ gt_usuario-pernr.


    SELECT *
     FROM zhcmt0007
     INTO CORRESPONDING FIELDS OF TABLE t_depart
      FOR ALL ENTRIES IN gt_usuario
       WHERE pernr EQ gt_usuario-pernr.


    SELECT a~objid a~objty b~kapid c~sobid d~pernr a~arbpl a~werks "F~KTEXT D~SNAME
    FROM crhd AS a
    INNER JOIN crca    AS b ON b~objid = a~objid
    INNER JOIN hrp1001 AS c ON c~objid = b~kapid
    INNER JOIN pa0001  AS d ON d~pernr = c~sobid
    INTO CORRESPONDING FIELDS OF TABLE gt_empregado
    FOR ALL ENTRIES IN gt_usuario
    WHERE d~pernr EQ gt_usuario-pernr
      AND c~otype EQ 'KA'.

    SELECT  *
      FROM ztpm_d_usuario
      INTO CORRESPONDING FIELDS OF TABLE gt_user
      FOR ALL ENTRIES IN gt_usuario
        WHERE pernr EQ gt_usuario-pernr.


*Atualizando a tabela deixando só empregados ativos.
    LOOP AT gt_usuario ASSIGNING FIELD-SYMBOL(<w_usuario>).
      READ TABLE s_pa0000 INTO DATA(_pa0000) WITH KEY pernr = <w_usuario>-pernr.
      IF sy-subrc = 0.
        <w_usuario>-ativo = abap_true.
      ENDIF.
    ENDLOOP.

*     Excluindo empregados desativados.
    DELETE gt_usuario WHERE ativo EQ abap_true.

*    Verificar se usuario esta em algum centro de trabalho ou se tem cadastro de login para acessar o sistema.
    LOOP AT gt_usuario INTO DATA(_d_usuario).
*      READ TABLE gt_empregado INTO DATA(w_empregado) WITH KEY pernr = _d_usuario-pernr.

      READ TABLE gt_user INTO gw_usuario WITH KEY pernr = _d_usuario-pernr.

      READ TABLE t_usuario INTO DATA(ls_usua) WITH KEY pernr =  _d_usuario-pernr.
      IF sy-subrc EQ 0.
        w_usuario = ls_usua-cname.
      ENDIF.

      READ TABLE t_depart INTO DATA(ls_dep) WITH KEY pernr =  _d_usuario-pernr.
      IF sy-subrc EQ 0.
        DATA(w_departamento)   = ls_dep-departamento.
        DATA(w_funcao)         = ls_dep-funcao.
      ENDIF.

      APPEND VALUE #(
                      pernr           = _d_usuario-pernr
                      cname           = w_usuario
                      cpf_nr          = gw_usuario-cpf_nr
                     departamento     = w_departamento
                      funcao          = w_funcao
                      login           = gw_usuario-login
                      ) TO t_user.

      CLEAR: gw_usuario, w_usuario, w_departamento, w_funcao.
    ENDLOOP.
  ENDMETHOD.


  METHOD me_registra_log.

    DATA: w_zpmt0052 TYPE zpmt0052.

    SELECT SINGLE * FROM zpmt0052 INTO w_zpmt0052 WHERE msgty EQ 'P'.
    IF sy-subrc EQ 0.
      w_zpmt0052-msgty = 'S'.
      w_zpmt0052-msgv1 = 'Concluído'.
      MODIFY zpmt0052 FROM w_zpmt0052.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD me_save_d_nota.
    DATA: w_zpmt0045 TYPE zpmt0045,
          t_zpmt0045 TYPE TABLE OF zpmt0045,
          w_zpmt0046 TYPE zpmt0046,
          t_zpmt0046 TYPE TABLE OF zpmt0046,
          t_zpmt0052 TYPE TABLE OF ZPMT0052.





    IF d_nota IS NOT INITIAL.
      "Limpar a tabelas.
      DELETE FROM zpmt0045.
      DELETE FROM zpmt0046.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      LOOP AT d_nota INTO DATA(w_nota).
        LOOP AT w_nota-part_objnr ASSIGNING FIELD-SYMBOL(<w_catalogo>).
          APPEND VALUE #( qmnum = w_nota-qmnum
                          qmart = w_nota-qmart
                          iwerk = w_nota-iwerk
                          otkat = <w_catalogo>-otkat
                          otgrp = <w_catalogo>-otgrp
                          oteil = <w_catalogo>-oteil
                          fekat = <w_catalogo>-fekat
                          fegrp = <w_catalogo>-fegrp
                          fecod = <w_catalogo>-fecod
                          urkat = <w_catalogo>-urkat
                          urgrp = <w_catalogo>-urgrp
                          urcod = <w_catalogo>-urcod
                          mnkat = <w_catalogo>-mnkat
                          mngrp = <w_catalogo>-mngrp
                          mncod = <w_catalogo>-mncod ) TO t_zpmt0046.
        ENDLOOP.

        APPEND VALUE #(  qmnum = w_nota-qmnum
                         iwerk = w_nota-iwerk
                         qmdat = w_nota-qmdat
                         tplnr = w_nota-tplnr
                         equnr = w_nota-equnr
*                         arjty = w_nota-arjty
*                         arjid = w_nota-arjid
                         ausvn = w_nota-ausvn
                         ausbs = w_nota-ausbs
                         auztv = w_nota-auztv
                         auztb = w_nota-auztb
                         strmn = w_nota-strmn
                         strur = w_nota-strur
                         ltrmn = w_nota-ltrmn
                         ltrur = w_nota-ltrur
*                         auszt = w_nota-auszt
*                         maueh = w_nota-maueh
                         btpln = w_nota-btpln
                         bequi = w_nota-bequi
                         qmart = w_nota-qmart
                         qmtxt = w_nota-qmtxt
*                         artpr = w_nota-artpr
                         priok = w_nota-priok
                         aufnr = w_nota-aufnr
                         msaus = w_nota-msaus
*                         sakat = w_nota-sakat
                         qmgrp = w_nota-qmgrp
                         qmcod = w_nota-qmcod
                         istat = w_nota-istat
                         txtnt = w_nota-txtnt ) TO t_zpmt0045.
      ENDLOOP.


      IF t_zpmt0045 IS NOT  INITIAL.
        MODIFY zpmt0045 FROM TABLE t_zpmt0045.
      ENDIF.

      IF t_zpmt0046 IS NOT  INITIAL.
        MODIFY zpmt0046 FROM TABLE t_zpmt0046.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF sy-subrc EQ 0.
        data(tipo) = 'S'.
        r_return = 'Dados da nota de manutenção concluido com sucesso'.
      ELSE.
        tipo = 'E'.
        r_return = 'Não foi possivél salvar as informações da nota de manutenção'.
      ENDIF.

    ELSE.
      tipo = 'E'.
      r_return = 'Não existe dados da nota para processar'.
    ENDIF.

     "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados da nota'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.
  ENDMETHOD.


  METHOD me_save_eqpto.

    DATA: t_zpmt0047 TYPE TABLE OF zpmt0047.
    DATA: t_zpmt0052 TYPE TABLE OF zpmt0052.
    DELETE FROM zpmt0047.
    COMMIT WORK.


    IF t_veiculos IS NOT INITIAL.
      MOVE-CORRESPONDING t_veiculos TO t_zpmt0047.

      "Grava dados na tabela.
      MODIFY zpmt0047 FROM TABLE t_zpmt0047.
      COMMIT WORK.

      IF sy-subrc EQ 0.
        DATA(tipo) = 'S'.
        r_return = 'Dados do equipamento concluido com sucesso.'.
      ELSE.
        tipo = 'E'.
        r_return = 'Não foi possivel salvar os dados do equipamento'.
      ENDIF.


      "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados do equipamento'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD me_save_local.
    DATA: t_zpmt0048 TYPE TABLE OF zpmt0048,
          t_zpmt0052 TYPE TABLE OF ZPMT0052.


    "Limpa a tabela.
    DELETE FROM zpmt0048.
    COMMIT WORK.


    IF t_local IS NOT INITIAL.
      MOVE-CORRESPONDING t_local TO t_zpmt0048.

      "Grava dados na tabela.
      MODIFY zpmt0048 FROM TABLE t_zpmt0048.
      COMMIT WORK.

      IF sy-subrc EQ 0.
        DATA(tipo) = 'S'.
        r_return = 'Dados de locais de instal concluido com sucesso'.
      ELSE.
        tipo = 'E'.
        r_return = 'Não foi possivel salvar dados de locais de intalação'.
      ENDIF.

      "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados locais de instalação'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD me_save_oper_ordem.
    DATA: t_zpmt0051 TYPE TABLE OF zpmt0051,
          t_zpmt0052 TYPE TABLE OF zpmt0052.

    CHECK t_operacao IS NOT INITIAL.
    FREE: t_zpmt0051.
    DELETE FROM zpmt0051.
    COMMIT WORK.

    MOVE-CORRESPONDING t_operacao TO t_zpmt0051.
    MODIFY zpmt0051 FROM TABLE t_zpmt0051.

    IF SY-SUBRC EQ 0.
      DATA(tipo) = 'S'.
      R_RETURN = 'Dados operações da ordem concluido com sucesso'.
    ELSE.
      tipo = 'E'.
      R_RETURN = 'Erro ao salvar operações da ordem'.
    ENDIF.

     "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados operações da ordem'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.
      CLEAR TIPO.
  ENDMETHOD.


  METHOD me_save_ordem.
    DATA: t_zpmt0049 TYPE TABLE OF zpmt0049,
          t_zpmt0052 TYPE TABLE OF zpmt0052.


    "Salva dados da ordem.
    DELETE FROM zpmt0049.
    COMMIT WORK.

    IF t_ordem IS NOT INITIAL.
      MOVE-CORRESPONDING t_ordem TO t_zpmt0049.

      MODIFY zpmt0049 FROM TABLE t_zpmt0049.
      COMMIT WORK.

      IF sy-subrc EQ 0.
        DATA(tipo) = 'S'.
        r_return = 'Dados da ordem concluido com sucesso.'.
      ELSE.
        tipo = 'E'.
        r_return = 'Não foi possivel salvar dados da ordem.'.
      ENDIF.

      "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados dados da ordem'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.
      CLEAR TIPO.
    ENDIF.
  ENDMETHOD.


  METHOD me_save_user.
    DATA: t_zpmt0050 TYPE TABLE OF zpmt0050,
          t_zpmt0052 TYPE TABLE OF zpmt0052.


    CHECK t_user IS NOT INITIAL.

    DELETE FROM zpmt0050.
    COMMIT WORK.

    FREE: t_zpmt0050.

    MOVE-CORRESPONDING t_user TO t_zpmt0050.
    MODIFY zpmt0050 FROM TABLE t_zpmt0050.
    COMMIT WORK.

    IF sy-subrc EQ 0.
      data(tipo) = 'S'.
      r_return = 'Dados de usuario concluido com sucesso.'.
    ELSE.
      tipo = 'S'.
      r_return = 'Não foi possivél salvar os usuarios.'.
    ENDIF.

    "Grava processo log.
      APPEND VALUE #(   mandt = sy-mandt
                        datum = sy-datum
                        uzeit = sy-uzeit
                        title = 'Processa dados dados do usuario'
                        uname = sy-uname
                        msgty = tipo
                        msgv1 = r_return ) TO t_zpmt0052.

      IF t_zpmt0052 IS NOT INITIAL.
        MODIFY zpmt0052 FROM TABLE t_zpmt0052.
        COMMIT WORK.
      ENDIF.
      CLEAR TIPO.
  ENDMETHOD.


  METHOD me_set_eqpto.
    "Selecina dados de equipmaento/veiculos e grava.
    zcl_mobile_manutencao=>me_get_eqpto(
    RECEIVING veiculos = DATA(t_veiculos)
    ).zcl_mobile_manutencao=>me_save_eqpto(
    EXPORTING t_veiculos =  t_veiculos RECEIVING r_return = DATA(return)
    ).

  ENDMETHOD.


  METHOD me_set_local.

    zcl_mobile_manutencao=>me_get_local(
    RECEIVING t_local = DATA(t_local)
    ).zcl_mobile_manutencao=>me_save_local(
    EXPORTING t_local = t_local RECEIVING r_return = DATA(return) ).
  ENDMETHOD.


  METHOD me_set_oper_ordem.

    zcl_mobile_manutencao=>me_get_oper_ordem(
    EXPORTING i_ordem = i_ordem
    RECEIVING t_operacao = DATA(t_operacao)
    ).zcl_mobile_manutencao=>me_save_oper_ordem(
    EXPORTING t_operacao =  t_operacao
    RECEIVING r_return = DATA(return) ).
  ENDMETHOD.


  METHOD me_set_ordem.

    DATA: i_data_de TYPE sy-datum.
    DATA: i_data_ate TYPE sy-datum.
    DATA: _data TYPE p DECIMALS 2.
    DATA: _date_interval TYPE rsis_t_range.".

    DATA: data_de  TYPE sy-datum,
          data_ate TYPE sy-datum.

    _data = 900. "Periodo de 360 dias.
    i_data_de = sy-datum - _data.
    i_data_ate = sy-datum.
    data_de   = i_data_de.
    data_ate  = i_data_ate.
    APPEND VALUE #( sign = 'I' option = 'BT'  low = i_data_de high = i_data_ate ) TO _date_interval.

    .zcl_mobile_manutencao=>me_get_ordem( EXPORTING data = _date_interval
    RECEIVING t_ordem = t_ordem
  ).zcl_mobile_manutencao=>me_save_ordem(
    EXPORTING t_ordem =  t_ordem RECEIVING r_return = DATA(return) ).
  ENDMETHOD.


  METHOD me_set_user.
    zcl_mobile_manutencao=>me_get_user(
     RECEIVING t_user = DATA(t_user)
     ).zcl_mobile_manutencao=>me_save_user(
    EXPORTING t_user =  t_user
    RECEIVING r_return = DATA(return) ).
  ENDMETHOD.


  method ZIF_MOBILE_MANUTENCAO~GET_INSTANCE.


    IF ZIF_MOBILE_MANUTENCAO~at_if_mobile_manutencao IS NOT BOUND.
      CREATE OBJECT ZIF_MOBILE_MANUTENCAO~AT_IF_MOBILE_MANUTENCAO TYPE ZCL_MOBILE_MANUTENCAO.
    ENDIF.

    R_IF_MOBILE_MANUTENCAO = ZIF_MOBILE_MANUTENCAO~AT_IF_MOBILE_MANUTENCAO.
  endmethod.


  METHOD zif_mobile_manutencao~zfi_set_nota_man.
    DATA: i_data_de TYPE sy-datum.
    DATA: i_data_ate TYPE sy-datum.
    DATA: _data TYPE p DECIMALS 2.
    DATA: _date_interval TYPE rsis_t_range.".

    DATA: data_de  TYPE sy-datum,
          data_ate TYPE sy-datum.

    _data = 360. "Periodo de 360 dias.
    i_data_de = sy-datum - _data.
    i_data_ate = sy-datum.
    data_de   = i_data_de.
    data_ate  = i_data_ate.
    APPEND VALUE #( sign = 'I' option = 'BT'  low = i_data_de high = i_data_ate ) TO _date_interval.

    zcl_mobile_manutencao=>me_get_d_nota(
      EXPORTING
        filial =  ' '   " Centro de planejamento de manutenção
        data   = _date_interval    " Tabela range genérica
      RECEIVING
        d_nota =  DATA(_d_nota)
        ).me_save_d_nota( EXPORTING d_nota =  _d_nota RECEIVING R_RETURN = R_RETURN ).

  ENDMETHOD.
ENDCLASS.
