*&---------------------------------------------------------------------*
*& Report  ZPMR0008
*& Impresso de planos de manutenção
*&---------------------------------------------------------------------*
*& Autor       : Marcos Faneli (Fontes Cleudo Ferreira)
*& Analista    : Cleudo Ferreira
*& Data criação: 23.10.2014
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Alterações:DEVK975356
*& Analista:Anderson Oenning
*& Data: 26/08/2017
*&---------------------------------------------------------------------*

REPORT  zpmr0008.

*** Macros
***********************************************************
DEFINE mc_preenche_fieldcat.
  APPEND VALUE #(
                 FIELDNAME     = &1
                 DATATYPE      = &2
                 REPTEXT_DDIC  = &3
                 OUTPUTLEN     = &4
                 DO_SUM        = &5
                 KEY           = &6
                 HOTSPOT       = &7
                 NO_ZERO       = &8
                 ) TO IT_FIELDCAT.
END-OF-DEFINITION.

DEFINE mc_preenche_class.
  VG_I = VG_I + 1.
  APPEND VALUE #(
                  SPOS      = VG_I
                  FIELDNAME = &1
                  GROUP     = &2
                  UP        = &3
                  SUBTOT    = &4
                 ) TO IT_SORT.

END-OF-DEFINITION.

DEFINE mc_preenche_cabec.
  APPEND VALUE #(
                 TYP  = &1
                 KEY  = &2
                 INFO = &3
                 ) TO IT_HEADER.
END-OF-DEFINITION.

*** Declaração de constantes
***********************************************************
CONSTANTS: cc_a TYPE c VALUE 'A',
           cc_x TYPE c VALUE 'X',
           cc_i TYPE c VALUE 'I',
           cc_1 TYPE c VALUE '1',
           cc_2 TYPE c VALUE '2'.
* Definições para ALV
TYPE-POOLS: kkblo.  "Tipos globais para ALV

*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*

DATA: v_tabix     TYPE sy-tabix        ,  " guardar o indice
      v_aedat     TYPE itob-aedat,
      v_tidnr     TYPE itob-tidnr,
      v_permissao TYPE c,
      v_bukrs(50) TYPE c,
      v_werks(50) TYPE c.

DATA: vg_i              TYPE i,
      st_fieldcat       TYPE kkblo_fieldcat,
      it_fieldcat       TYPE kkblo_t_fieldcat,
      st_colinfo        TYPE kkblo_specialcol,
      st_layout         TYPE kkblo_layout,
      st_print          TYPE slis_print_alv,
      st_header         TYPE kkblo_listheader,
      it_fieldcat_alv   TYPE slis_t_fieldcat_alv,
      it_special_groups TYPE slis_t_sp_group_alv,
      it_layout_alv     TYPE slis_layout_alv,
      it_header         TYPE kkblo_t_listheader,
      it_sort           TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      st_grid_settings  TYPE lvc_s_glay.

INCLUDE <icon>.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
DATA: o_alv       TYPE REF TO cl_gui_alv_grid,
      o_cont      TYPE REF TO cl_gui_custom_container,
      def_variant TYPE disvariant,
      variant     TYPE disvariant,
      v_save(1)   TYPE c VALUE 'A'.

*----------------------------------------------------*
*               Tables                               *
*----------------------------------------------------*

TABLES: viqmel, caufv, equz, iflo, eqkt, imptt, mpla, afih, mhis.

*----------------------------------------------------*
*               Types                                *
*----------------------------------------------------*
TYPES:
  BEGIN OF ty_zpmr0008,
    bukrs    TYPE c LENGTH 50,
    werks    TYPE c LENGTH 50,
    equnr    TYPE equi-equnr,
    auart    TYPE caufv-auart,
    aufnr    TYPE caufv-aufnr,
    tidnr    TYPE equz-tidnr,
    erdat    TYPE caufv-erdat,
    idat1    TYPE caufv-idat1,
    warpl    TYPE mpla-warpl,
    wptxt    TYPE mpla-wptxt,
    bautl    TYPE mpos-bautl,
    maktx    TYPE makt-maktx,
    aufpl    TYPE caufv-aufpl,
    aplzl    TYPE afvc-aplzl,
    ltxa1    TYPE afvc-ltxa1,
    ltxa2    TYPE afvc-ltxa2,
    gstrp    TYPE caufv-gstrp,
    equnrdes TYPE c LENGTH 50,
  END OF ty_zpmr0008,

  BEGIN OF ty_caufv ,         " Ordem
    aufnr TYPE caufv-aufnr, " Codigo da ordem
    bukrs TYPE caufv-bukrs, " Empresa
    werks TYPE caufv-werks, " Centro
    erdat TYPE caufv-erdat, " Data criação
    idat1 TYPE caufv-idat1, " Data liberação
    ktext TYPE caufv-ktext, " Texto
    objnr TYPE caufv-objnr, " Equipamento
    auart TYPE caufv-auart, " Tipo de Ordem
    gstrp TYPE caufv-gstrp, " Data planejamento Ordem
    gltrp TYPE caufv-gltrp, " Fim Base
    idat3 TYPE caufv-idat3, " Data encerramento
    tplnr TYPE viaufkst-tplnr, "Local
    pltxt TYPE iflo-pltxt,
  END OF ty_caufv,

  BEGIN OF ty_viaufks,         " Relacionamento ordem x operações
    aufnr TYPE viaufks-aufnr, " Codigo da ordem
    aufpl TYPE viaufks-aufpl, " Nº de roteiro de operações na ordem
    tplnr TYPE viaufks-tplnr,
  END OF ty_viaufks,

  BEGIN OF ty_eqkt,
    equnr TYPE eqkt-equnr,
    eqktx TYPE eqkt-eqktx,
  END OF ty_eqkt,

  BEGIN OF ty_iflo,
    tplnr TYPE iflo-tplnr,
    pltxt TYPE iflo-pltxt,
  END OF ty_iflo,

  BEGIN OF ty_jest ,           " Status da Ordem
    objnr TYPE jest-objnr,   " Objeto
    stat  TYPE jest-stat,    " Nº do objeto
    txt04 TYPE tj02t-txt04,  " Descrição dos status
  END OF ty_jest,

  BEGIN OF ty_afvc ,         " Lista de operações
    aufpl TYPE afvc-aufpl, " Nº de roteiro de operações na ordem
    aplzl TYPE afvc-aplzl, " Numerador geral da ordem
    ltxa1 TYPE afvc-ltxa1, " Descrição das atividades.
    ltxa2 TYPE afvc-ltxa1, " Descrição das atividades.
    vornr TYPE afvc-vornr,
  END OF ty_afvc,

  BEGIN OF ty_mpla ,          " Plano de manutenção
    warpl  TYPE mpla-warpl,  " Código do plano
    aufnr  TYPE caufv-aufnr, " Codigo da ordem
    ersdt  TYPE mpla-ersdt,  " Data criação
    wptxt  TYPE mpla-wptxt,  " Descrição
    bautl  TYPE mpos-bautl,  " Conjunto
    maktx  TYPE makt-maktx,  " Texto do conjunto
    warpl1 TYPE viqmel-warpl, "Plano da nota de manutenção
    qmnum  TYPE viqmel-qmnum, " N nota de manutenção
    nplda  TYPE mhis-nplda,   " Data planejamento
    objnr  TYPE mpla-objnr,
  END OF ty_mpla,


  BEGIN OF ty_equi ,          " Equipamento
    aufnr TYPE caufv-aufnr, " Codigo da ordem
    tplnr TYPE iflo-tplnr,
    equnr TYPE equi-equnr,  " Codigo do equipamento
    eqart TYPE equi-eqart,  " Tipo técnico
    herst TYPE equi-herst,  " Marca
    typbz TYPE equi-typbz,  " Modelo
    qmnum TYPE viqmel-qmnum, " Codigo da nota
  END OF ty_equi,

  BEGIN OF ty_equi_aux ,      " Equipamento
    equnr TYPE equi-equnr,  " Codigo do equipamento
  END OF ty_equi_aux,

  BEGIN OF ty_mhis ,          " Histórico dos planos
    aufnr TYPE caufv-aufnr, " Codigo da ordem
    warpl TYPE mhis-warpl , " numero
    abnum TYPE mhis-abnum , " sequencia
    nplda TYPE mhis-nplda , " Data Exec Ordem
    horda TYPE mhis-horda , " Data Solicitação
    qmnum TYPE viqmel-qmnum, "Codigo da nota

  END OF ty_mhis,

  BEGIN OF ty_t001w ,         " Centro
    werks TYPE t001w-werks, " codigo
    name1 TYPE t001w-name1, " descrição
    iwerk TYPE t001w-iwerk, " Centro planejamento
  END OF ty_t001w,

  BEGIN OF ty_imrg ,         "
    equnr TYPE equi-equnr, " codigo
    idate TYPE imrg-idate, " descrição
    itime TYPE imrg-itime, "
*          tplnr TYPE tplnr     , " Local de abastecimento
  END OF ty_imrg,

  BEGIN OF ty_equz,  " equz: Intervalo de tempo equipamento
    equnr TYPE equz-equnr,  " Nº equipamento
    datbi TYPE equz-datbi,  " Data de validade final
    iwerk TYPE equz-iwerk,  " Centro de planejamento de manutenção
    tidnr TYPE equz-tidnr,  " Nº identificação técnica
  END OF ty_equz,

  BEGIN OF ty_param,
    param TYPE ztparam-param,
    zval  TYPE ztparam-zval,
    const TYPE ztparam-const,
  END OF ty_param.

TYPES: BEGIN OF ty_zval,
         param TYPE ztparam-param,
         zval  TYPE caufv-auart,
         const TYPE caufv-werks,
       END OF ty_zval,

       BEGIN OF ty_relatorio,      " Dados do relatorio
         mark,
         bukrs(50)    TYPE c                      , " Empresa
         werks(50)    TYPE c                      , " Centro
         warpl        TYPE mpla-warpl             , " Código do plano
         wptxt        TYPE mpla-wptxt             , " Descrição
         aufnr        TYPE caufv-aufnr            , " Codigo da ordem
         auart        TYPE caufv-auart            , " Tipo de ordem
         erdat        TYPE caufv-erdat            , " Data criação
         gstrp        TYPE caufv-gstrp            , " Data planejamento Ordem
         idat1        TYPE caufv-idat1            , " Data liberação
         ktext        TYPE caufv-ktext            , "Texto
         equnr        TYPE equi-equnr             , " Codigo do equipamento
         equnrdes(50) TYPE c                      , " equi-EQUNR             , " Codigo do equipamento
         tidnr        TYPE itob-tidnr             , " Identificação tecnica
         nplda        TYPE mhis-nplda             , " Data planej. Plano
         bautl        TYPE mpos-bautl             , " Conjunto
         maktx        TYPE makt-maktx             , " Texto do conjunto
         iwerk(50)    TYPE c                      , " Centro da nota
         equnr1       TYPE viqmel-equnr           , " Equipamento Nota
         warpl1       TYPE viqmel-warpl           , " Plano nota
         qmtxt        TYPE viqmel-qmtxt           , " Texto plano Nota
         qmnum        TYPE viqmel-qmnum           , " N° Nota
         qmart        TYPE viqmel-qmart           , " Tipo Nota
         erdat1       TYPE viqmel-erdat           , " Data Nota
         ersdt        TYPE mpla-ersdt             , "Data ultimo planejamento
         qmdat        TYPE viqmel-qmdat           , "Data planejamento da nota
         tplnr        TYPE viaufks-tplnr          , "Local de instalação
         pltxt        TYPE iflo-pltxt             , "Descrição do Local
         eqkt         TYPE eqkt-eqktx             , "Descrição do equpamento.
         gltrp        TYPE caufv-gltrp            , "Fim Base
       END OF ty_relatorio,

       BEGIN OF ty_viqmel, "REQUEST DEVK970903
         tplnr  TYPE viqmel-tplnr,
         equnr  TYPE viqmel-equnr,  "Equipamento
         iwerk  TYPE viqmel-iwerk,  "Centro de planejameto
         erdat  TYPE viqmel-erdat,  " Data da nota
         qmnum  TYPE viqmel-qmnum,  " Nota manutenção
         qmtxt  TYPE viqmel-qmtxt,  "Texto breve nota
         qmart  TYPE viqmel-qmart,  " Tipo de nota de manutenção
         bautl  TYPE viqmel-bautl,  "Conjunto
         warpl  TYPE viqmel-warpl,  "Plano de manutenção
         beber  TYPE viqmel-beber,  "Area operacional
         objnr  TYPE viqmel-objnr,  " Equipamento
         bukrs  TYPE viqmel-bukrs,  "Empresa
         qmdat  TYPE viqmel-qmdat,  "Data da nota
         warpl1 TYPE mpla-warpl  ,  " Código do plano
         wptxt  TYPE mpla-wptxt  ,  " Descrição
       END OF ty_viqmel,

       BEGIN OF ty_carac,
         equnr TYPE equi-equnr, "Equipamento
         atinn TYPE cabnt-atinn, "Cod caracteristica
         atbez TYPE cabnt-atbez, "Desc caracteristica
       END OF ty_carac,

       BEGIN OF ty_local,
         equnr TYPE equi-equnr, "Equipamento
         tplnr TYPE iflo-tplnr, "Local instalação
         pltxt TYPE iflo-pltxt, "Descrição do local de instalação
       END OF ty_local.

*----------------------------------------------------*
*                Tabelas Internas                    *
*----------------------------------------------------*
DATA: t_caufv      TYPE TABLE OF ty_caufv    WITH HEADER LINE,
      t_imrg       TYPE TABLE OF ty_imrg     WITH HEADER LINE,
      t_mpla       TYPE TABLE OF ty_mpla     WITH HEADER LINE,
      t_equi       TYPE TABLE OF ty_equi     WITH HEADER LINE,
      t_viaufks    TYPE TABLE OF ty_viaufks WITH HEADER LINE,
      t_equi_aux   TYPE TABLE OF ty_equi_aux,
      t_t001w      TYPE TABLE OF ty_t001w    WITH HEADER LINE,
      t_afvc       TYPE TABLE OF ty_afvc     WITH HEADER LINE,
      t_jest       TYPE TABLE OF ty_jest     WITH HEADER LINE,
      t_mhis       TYPE TABLE OF ty_mhis     WITH HEADER LINE,
      w_mhis       TYPE ty_mhis,
      t_equz       TYPE TABLE OF ty_equz     WITH HEADER LINE,
      t_zpmr0008c  TYPE TABLE OF zpmr0008    WITH HEADER LINE,  " Auxilar do impresso - sumarizado
      t_zpmr0008c2 TYPE TABLE OF zpmr0008    WITH HEADER LINE,  " Auxilar do impresso - sumarizado
      t_zpmr0008i  TYPE TABLE OF zpmr0008    WITH HEADER LINE,  " Auxilar do impresso - analitico
      t_relatorio  TYPE TABLE OF ty_relatorio,
      w_relatorio  TYPE ty_relatorio,
      t_t001       TYPE TABLE OF t001,
      w_t001       TYPE t001,
      w_t001w      TYPE t001w,
      t_viqmel     TYPE TABLE OF  ty_viqmel WITH HEADER LINE,
      w_viqmel     TYPE ty_viqmel,
      t_carac      TYPE TABLE OF ty_carac WITH HEADER LINE,
      w_carac      TYPE ty_carac,
      t_local      TYPE TABLE OF ty_local WITH HEADER LINE,
      w_local      TYPE ty_local,
      t_iflo       TYPE TABLE OF ty_iflo WITH HEADER LINE,
      t_eqkt       TYPE TABLE OF ty_eqkt WITH HEADER LINE,
      it_param     TYPE TABLE OF ty_param WITH HEADER LINE,
      it_zval      TYPE TABLE OF ty_zval   WITH HEADER LINE.

DATA: ti_linecolor  TYPE slis_specialcol_alv   OCCURS 0 WITH HEADER LINE,
      ti_listheader TYPE slis_t_listheader,
      ti_fieldcat   TYPE slis_t_fieldcat_alv            WITH HEADER LINE,
      ti_sort       TYPE slis_sortinfo_alv     OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------*
*               Variaveis Globais                    *
*----------------------------------------------------*
DATA: v_listheader TYPE slis_listheader,    "Cabeçalho
      v_layout     TYPE slis_layout_alv,    "layout para saída
      v_print      TYPE slis_print_alv,     "Ctrl de impressão
      v_variante   LIKE disvariant.         "Variante de exibiçã

DATA: w_events     TYPE slis_t_event WITH HEADER LINE, " Work-area eventos
      w_es_variant LIKE disvariant,      " variants.
      w_layout     TYPE slis_layout_alv,      " Para layout tipo zebra
      i_events     TYPE slis_t_event,         " Para os eventos
      v_repid      LIKE sy-repid,
      v_flag,
      v_lnumt(25)  TYPE c.

DATA: t_makt TYPE TABLE OF makt WITH HEADER LINE.

CONSTANTS:
  l_or_prevent(12) VALUE 'OR_PREVENT',
  l_or_lubrif(12)  VALUE 'OR_LUBRIF'.


*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_werks FOR caufv-werks OBLIGATORY ,
                p_gstrp FOR caufv-gstrp OBLIGATORY,
                p_qmart FOR viqmel-qmart,
                p_auart FOR caufv-auart ,
                p_tplnr FOR iflo-tplnr,
                p_equnr FOR t_equi-equnr,
                p_eqart FOR t_equi-eqart,
                p_bautl FOR afih-bautl,
                p_tidnr FOR equz-tidnr.
SELECTION-SCREEN ULINE.
PARAMETERS:     p_vari  TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK block1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETER: p_equi RADIOBUTTON GROUP b2 DEFAULT 'X',
           p_plan RADIOBUTTON GROUP b2.
SELECTION-SCREEN: END OF BLOCK b3.

*---------------------------------------------------*
*               Evento de inicialização             *
*---------------------------------------------------*
INITIALIZATION.

  variant-report = 'ZPMR0008'.
  def_variant-report = 'ZPMR0008'.

* Verificar se existe uma variante default
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc IS INITIAL.
    p_vari = def_variant-variant.
  ENDIF.

*---------------------------------------------------*
*          Evento de seleção de variant             *
*---------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f_f4_variant.

*---------------------------------------------------*
*Evento para verificar se existe a variant informada*
*---------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM f_variant.

*----------------------------------------------------*
*        Evento: Start-of-Selection                  *
*----------------------------------------------------*
START-OF-SELECTION.

  IF p_qmart IS NOT INITIAL.
    PERFORM selec_dados_not.
  ELSEIF
    p_auart IS NOT INITIAL.
    PERFORM seleciona_dados.
  ELSE.
    MESSAGE 'Adicionar tipo de nota ou tipo ordem de manutenção.' TYPE 'I'.
    EXIT.
  ENDIF.

  IF p_auart IS NOT INITIAL AND t_mpla[] IS NOT INITIAL.
    PERFORM prepara_relatorio.

  ELSEIF p_qmart IS NOT INITIAL.
    PERFORM prepara_relatorio_nota.

  ELSE.
    IF v_permissao = 'S'.

      MESSAGE 'Sem permissão para os critérios selecionados.' TYPE 'I'.
    ELSE.
      MESSAGE 'Não encontrados registros.' TYPE 'I'.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  IF t_relatorio[] IS NOT INITIAL.
    PERFORM f_cabecalho.
    PERFORM f_catalogo.
    PERFORM f_classificacao.
    PERFORM f_layout.
    PERFORM eventos.
    PERFORM f_relatorio TABLES t_relatorio.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  seleciona_dados
*&---------------------------------------------------------------------*

FORM seleciona_dados.

  FREE: t_caufv, t_mpla, t_equi, t_t001w, t_relatorio, t_afvc, t_jest, t_mhis, t_zpmr0008c, t_zpmr0008c2, t_zpmr0008i, t_makt.
  CLEAR: v_permissao, t_makt.

  PERFORM f_lupa USING 'Selecionando ordens de manutenção' space.

  FREE: it_zval[].
  SELECT param zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param
    WHERE param IN (l_or_prevent,l_or_lubrif).

  IF NOT it_param[] IS INITIAL.
    LOOP AT it_param.
      it_zval-param = it_param-param.
      it_zval-zval =  it_param-zval.
      it_zval-const = it_param-const.
      APPEND it_zval.
    ENDLOOP.
  ENDIF.

  IF it_zval[] IS NOT INITIAL.

* Busca ordem de manutenção
    SELECT a~aufnr a~bukrs a~werks a~erdat a~idat1 a~ktext a~objnr a~auart a~gstrp a~gltrp a~idat3 b~tplnr c~pltxt
    FROM  caufv AS a
    INNER JOIN viaufkst AS b ON b~aufnr = a~aufnr
    INNER JOIN iflo     AS c ON c~tplnr = b~tplnr
    INTO CORRESPONDING FIELDS OF TABLE t_caufv
    FOR ALL ENTRIES IN it_zval
      WHERE a~auart EQ it_zval-zval
        AND a~werks IN p_werks
        AND a~erdat IN p_gstrp     " DATA DA ENTRADA
        AND a~auart IN p_auart
        AND b~tplnr IN p_tplnr
        AND a~werks EQ it_zval-const.
  ENDIF.

  CHECK t_caufv[] IS NOT INITIAL.

  IF p_equnr[] IS NOT INITIAL AND p_tidnr[] IS NOT INITIAL.
    MESSAGE s899(mm) WITH 'Informar equipamento ou identificação técnica.' DISPLAY LIKE 'E'.
  ENDIF.

*  IF P_EQUNR[] IS NOT INITIAL.
  PERFORM f_lupa USING 'Selecionando equipamento(s) das ordens de manutenção' space.
* Busca equipamento das ordens de manutenção
  SELECT viaufkst~aufnr equi~eqart equi~equnr equi~herst equi~typbz viaufkst~tplnr
    INTO CORRESPONDING FIELDS OF TABLE t_equi
    FROM  viaufkst
    INNER JOIN equi ON equi~equnr = viaufkst~equnr
    FOR ALL ENTRIES  IN t_caufv
    WHERE aufnr      EQ t_caufv-aufnr
     AND  viaufkst~equnr IN p_equnr
     AND  equi~eqart IN p_eqart.

*    CHECK T_EQUI[] IS NOT INITIAL.

  IF t_equi[] IS NOT INITIAL.
    SELECT equnr eqktx
      INTO TABLE t_eqkt
      FROM eqkt
      FOR ALL ENTRIES IN t_equi
      WHERE equnr EQ t_equi-equnr.

* Busca equipamentos através da identificação técnica
    SELECT equz~equnr equz~datbi equz~iwerk equz~tidnr
      INTO CORRESPONDING FIELDS OF TABLE t_equz
      FROM equz
      FOR ALL ENTRIES IN t_equi
      WHERE equz~equnr EQ t_equi-equnr
       AND  equz~tidnr IN p_tidnr
       AND  equz~iwerk IN p_werks.

* Elimina equipamentos que não tem identificação técnica.
    LOOP AT t_equi.
      v_tabix = sy-tabix.
      IF NOT line_exists( t_equz[ equnr = t_equi-equnr ] ).
        DELETE t_equi INDEX v_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM f_lupa USING 'Selecionando status' space.

* Busca os status
  SELECT jest~objnr jest~stat tj02t~txt04
    INTO TABLE t_jest
    FROM jest
      INNER JOIN tj02t ON tj02t~istat = jest~stat
        FOR ALL ENTRIES IN t_caufv
          WHERE jest~objnr  EQ t_caufv-objnr
            AND jest~inact  EQ abap_false   " Somente os ativos
            AND tj02t~spras EQ sy-langu.

* Deleta os status que não forem o de concluído.
  DELETE t_jest WHERE txt04 NE 'ENTE'
                AND   txt04 NE 'NEXE'
                AND   txt04 NE 'ENCE'
                AND   txt04 NE 'ABER'.

  PERFORM f_lupa USING 'Selecionando planos manutenção' space.
* Busca planos de Manutenção

  SELECT mpla~warpl viaufkst~aufnr mpla~ersdt mpla~wptxt mpos~bautl
    INTO TABLE t_mpla
    FROM viaufkst
    INNER JOIN mpla ON mpla~warpl = viaufkst~warpl
    INNER JOIN mpos ON mpos~warpl = viaufkst~warpl
    FOR ALL ENTRIES IN t_caufv
    WHERE viaufkst~aufnr EQ t_caufv-aufnr
     AND  mpos~bautl IN p_bautl.

  PERFORM f_lupa USING 'Atualizando conjuntos' space.

  CHECK t_mpla[] IS NOT INITIAL.

* Busca descrição do conjunto
  SELECT *
    INTO TABLE t_makt
    FROM makt
    FOR ALL ENTRIES IN t_mpla
    WHERE makt~spras EQ sy-langu
      AND makt~matnr EQ t_mpla-bautl.

* Atualiza descrição do conjunto
  LOOP AT t_mpla ASSIGNING FIELD-SYMBOL(<mpla>).
    IF line_exists( t_makt[ matnr = <mpla>-bautl ] ).
      <mpla>-maktx = t_makt[ matnr = <mpla>-bautl ]-maktx.
    ENDIF.
  ENDLOOP.

  PERFORM f_lupa USING 'Selecionando histórico dos planos de manutenção' space.
  " Busca histórico do plano de manutenção

  SELECT afih~aufnr mhis~warpl mhis~abnum mhis~nplda mhis~horda
  INTO TABLE t_mhis
  FROM afih
  INNER JOIN mhis ON  mhis~warpl = afih~warpl
                  AND mhis~abnum = afih~abnum
  FOR ALL ENTRIES IN t_caufv
  WHERE aufnr EQ t_caufv-aufnr.

  PERFORM f_lupa USING 'Eliminando ordens sem planos de manutenção' space.
* Delete as ordens que não forem de planos
  LOOP AT t_caufv.
    v_tabix = sy-tabix.

*   Delete as ordens que não forem do plano de manutenção
    IF NOT line_exists( t_mpla[ aufnr = t_caufv-aufnr ] ).
      DELETE t_caufv INDEX v_tabix. CONTINUE.
    ENDIF.

    PERFORM f_lupa USING 'Eliminando ordens encerradas' space.
*   Deleta as ordens encerradas.
    IF line_exists( t_jest[ objnr = t_caufv-objnr ] ).
      DELETE t_caufv INDEX v_tabix. CONTINUE.
    ENDIF.

*    IF T_EQUI[] IS NOT INITIAL.
**   Deleta as ordens dos equipamentos não selecionados.
*      IF NOT LINE_EXISTS( T_EQUI[ AUFNR = T_CAUFV-AUFNR ] ).
*        DELETE T_CAUFV INDEX V_TABIX. CONTINUE.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

  CHECK t_caufv[] IS NOT INITIAL.

* Busca empresa
  SELECT DISTINCT * INTO w_t001 FROM  t001
    FOR ALL ENTRIES IN t_caufv
  WHERE bukrs EQ t_caufv-bukrs.
  ENDSELECT.

* Busca centro
  SELECT SINGLE *
    INTO w_t001w
    FROM t001w
    WHERE werks IN p_werks.

  PERFORM f_lupa USING 'Relacionando ordens com operações' space.

* Relacionamento ordem com operações
  SELECT DISTINCT aufnr aufpl tplnr "Incluir ordem na tabela VIAUFKS / AOENNING
    INTO TABLE t_viaufks
      FROM viaufks
      FOR ALL ENTRIES IN t_caufv
      WHERE aufnr EQ t_caufv-aufnr.

  IF sy-subrc IS INITIAL.
*   Lista de operações
    SELECT aufpl aplzl ltxa1 ltxa2 vornr
      INTO TABLE t_afvc
        FROM afvc
        FOR ALL ENTRIES IN t_viaufks
        WHERE aufpl  EQ t_viaufks-aufpl.
  ENDIF.

* Busca Centro
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_t001w
      FROM t001w
      FOR ALL ENTRIES IN t_caufv
      WHERE werks EQ t_caufv-werks.

*   Seleção da descrição do Local / AOENNING
  SELECT tplnr pltxt
    FROM iflo
      INTO TABLE t_iflo
      FOR ALL ENTRIES IN t_viaufks
      WHERE tplnr EQ t_viaufks-tplnr.

  CHECK t_iflo[] IS NOT INITIAL.
  CHECK t_equi[] IS NOT INITIAL.

* Axiliar de Cliente para o select da imrg abaixo
  SELECT DISTINCT equnr
    INTO TABLE t_equi_aux
      FROM equi
      FOR ALL ENTRIES IN  t_equi
      WHERE equnr = t_equi-equnr.

  CHECK t_equi_aux[] IS NOT INITIAL.

* Grava dados na tabela ztpm_lubri_imrg
  SELECT DISTINCT equi~equnr imrg~idate imrg~itime
  INTO TABLE t_imrg
  FROM  equi
  INNER JOIN imptt ON  imptt~mpobj = equi~objnr
                   AND imptt~mptyp = 'M'
                   AND imptt~inact = abap_false
                   AND imptt~indtr = abap_false
  INNER JOIN imrg ON imrg~point = imptt~point
  FOR ALL ENTRIES IN t_equi_aux
  WHERE  equi~equnr = t_equi_aux-equnr.

ENDFORM.                    " seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  prepara_relatorio
*&---------------------------------------------------------------------*
FORM prepara_relatorio.

  DATA: v_qtde  TYPE p DECIMALS 2,
        v_tabix TYPE p DECIMALS 2.

  SORT t_imrg BY equnr ASCENDING idate DESCENDING itime DESCENDING.

  DESCRIBE TABLE t_caufv LINES v_qtde.
  CLEAR w_relatorio.

  LOOP AT t_caufv.

    CLEAR: t_mpla.

    v_tabix = ( sy-tabix / v_qtde ) * 100 .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = v_tabix
        text       = 'Preparando relatório'.

    TRY .
        t_mpla = t_mpla[ aufnr = t_caufv-aufnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_mpla.
    ENDTRY.

    TRY .
        t_viaufks = t_viaufks[ aufnr = t_caufv-aufnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_viaufks.
    ENDTRY.


    TRY .
        t_iflo = t_iflo[ tplnr = t_viaufks-tplnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_iflo.
    ENDTRY.

    TRY .
        t_equi = t_equi[ aufnr = t_caufv-aufnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_equi.
    ENDTRY.

    CLEAR: v_aedat, v_tidnr.

    SELECT SINGLE aedat tidnr
      INTO (v_aedat , v_tidnr)
      FROM equz
     WHERE equnr EQ t_equi-equnr.

    TRY .
        t_eqkt = t_eqkt[ equnr = t_equi-equnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_eqkt.
    ENDTRY.

    TRY .
        t_mhis = t_mhis[ aufnr = t_caufv-aufnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_mhis.
    ENDTRY.

    APPEND VALUE #(
                      bukrs    = t_caufv-bukrs
                      werks    = |{ t_t001w[ werks = t_caufv-werks ]-werks ALPHA = OUT }-{ t_t001w[ werks = t_caufv-werks ]-name1 }|
                      aufnr    = |{ t_caufv-aufnr ALPHA = OUT }|  " Codigo da ordem
                      auart    = t_caufv-auart  " Tipo de ordem
                      erdat    = t_caufv-erdat  " Data criação
                      gstrp    = t_caufv-gstrp  " Data Planej. Ordem
                      idat1    = t_caufv-idat1  " Data liberação
                      ktext    = t_caufv-ktext  " Texto
                      warpl    = |{ t_mpla-warpl ALPHA = OUT }|   " Código do plano
                      wptxt    = t_mpla-wptxt " Descrição
                      bautl    = |{ t_mpla-bautl ALPHA = OUT }| " Conjunto
                      maktx    = t_mpla-maktx " Texto do conjunto
                      tplnr    = t_caufv-tplnr
                      pltxt    = t_caufv-pltxt
                      tidnr    = v_tidnr
                      equnrdes = condense( |{ t_equi-equnr ALPHA = OUT }-{ t_eqkt-eqktx }| )
                      equnr    = |{ t_equi-equnr ALPHA = OUT }|
                      nplda    = t_mhis-nplda " Data Exec Ordem
                  ) TO t_relatorio.

  ENDLOOP.

  SORT t_relatorio BY  werks equnr warpl aufnr.

ENDFORM.                    " prepara_relatorio

FORM prepara_relatorio_nota.

  DATA: v_qtde  TYPE p DECIMALS 2,
        v_tabix TYPE p DECIMALS 2.

  SORT t_imrg BY equnr ASCENDING idate DESCENDING itime DESCENDING.

  DESCRIBE TABLE t_viqmel LINES v_qtde.

  LOOP AT t_viqmel.

    v_tabix = ( sy-tabix / v_qtde ) * 100 .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = v_tabix
        text       = 'Preparando relatório'.

    TRY .
        t_t001w = t_t001w[ iwerk = t_viqmel-iwerk ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_t001w.
    ENDTRY.

    TRY .
        t_mhis = t_mhis[ warpl = t_viqmel-warpl ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_mhis.
    ENDTRY.

    TRY .
        t_mpla = t_mpla[ warpl = t_viqmel-warpl ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_mpla.
    ENDTRY.

    APPEND VALUE #(
                      tplnr = t_viqmel-tplnr
                      equnr = t_viqmel-equnr
                      warpl = t_viqmel-warpl
                      qmnum = |{ t_viqmel-qmnum ALPHA = OUT }|
                      qmart = t_viqmel-qmart
                      erdat = t_viqmel-erdat
                      qmdat = t_viqmel-qmdat
                      iwerk = |{ t_t001w-iwerk }-{ t_t001w-name1 }|
                      nplda = t_mhis-nplda
                      wptxt = t_mpla-wptxt
                  ) TO t_relatorio.

  ENDLOOP.

  SORT t_relatorio BY iwerk qmnum equnr warpl.

ENDFORM.
************************************************************************
***                        Definições ALV                            ***
************************************************************************
*&--------------------------------------------------------------------*
*&      Form  top_of_page
*&--------------------------------------------------------------------*
*       Imprime o cabeçalho
*---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
      i_logo             = 'LOGO_NOVO'.
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
      selfield TYPE slis_selfield.

  CASE ucomm.
    WHEN 'PRINT'.

      PERFORM prepara_selecionados.
      IF t_zpmr0008i[] IS NOT INITIAL.
        PERFORM imprime_selecionados.
      ENDIF.

      IF NOT p_qmart IS INITIAL.
        PERFORM imprime_selecionados_notas.
      ENDIF.

    WHEN '&IC1'.
      IF selfield-fieldname = 'WARPL'.
        SET PARAMETER ID 'MPL' FIELD selfield-value.
        CALL TRANSACTION 'IP03' AND SKIP FIRST SCREEN .
      ENDIF.

      IF selfield-fieldname = 'QMNUM'.
        SET PARAMETER ID 'IQM' FIELD selfield-value.
        CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN .
      ENDIF.

      IF selfield-fieldname = 'EQUNR'.
        SET PARAMETER ID 'EQN' FIELD selfield-value.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
      ENDIF.

*    when 'MAR'.
*
*      PERFORM MARCAR_TODOS_REGISTROS.
*
*    when 'DES'.
*
*      PERFORM DESMARCA_TODOS_REGISTROS.

    WHEN OTHERS.
  ENDCASE.
  selfield-refresh = 'X'.
ENDFORM.                    " user_command

*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS '900'.

ENDFORM.                    "pf_status_set
*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cabecalho.

  DATA: l_cabec(150)  TYPE c,
        l_cabec1(150) TYPE c,
        l_cabec2(150) TYPE c.

  mc_preenche_cabec: 'H' ' ' 'Dados de Seleção',  " Título do relatório
                     'S' ' ' ' '.                 " Para não exibir barra de rolagem

** Centro
*  IF P_WERKS IS NOT INITIAL  .
*    L_CABEC = P_WERKS.
*  ELSE.
*    L_CABEC = 'Todos'.
*  ENDIF.
*  MC_PREENCHE_CABEC:  'S' 'Centro              ' L_CABEC.
* Tipo de Ordem
  IF ( p_auart-low IS NOT INITIAL ) AND ( p_auart-high IS NOT INITIAL ).
    CONCATENATE p_auart-low 'a' p_auart-high INTO l_cabec SEPARATED BY space.
  ELSEIF ( p_auart-low IS NOT INITIAL ).
    l_cabec = p_auart-low.
  ELSE.
    l_cabec = ' '.
  ENDIF.

  IF p_auart IS NOT INITIAL.
    mc_preenche_cabec:  'S' 'Tipo de Ordem       ' l_cabec.
  ENDIF.

  "Tipo de nota
  IF ( p_qmart-low IS NOT INITIAL ) AND ( p_qmart-high IS NOT INITIAL ).
    CONCATENATE p_qmart-low 'a' p_qmart-high INTO l_cabec SEPARATED BY space.
  ELSEIF ( p_qmart-low IS NOT INITIAL ).
    l_cabec = p_qmart-low.
    l_cabec = p_qmart-low.
  ELSE.
    l_cabec = ' '.
  ENDIF.

  IF p_qmart IS NOT INITIAL.
    mc_preenche_cabec:  'S' 'Tipo de Nota       ' l_cabec.
  ENDIF.

* Equipamento
  IF ( p_equnr-low IS NOT INITIAL ) AND ( p_equnr-high IS NOT INITIAL ).
    CONCATENATE p_equnr-low 'a' p_equnr-high INTO l_cabec SEPARATED BY space.
  ELSEIF ( p_equnr-low IS NOT INITIAL ).
    l_cabec = p_equnr-low.
  ELSE.
    l_cabec = ' '.
  ENDIF.

  IF p_equnr IS NOT INITIAL.
    mc_preenche_cabec:  'S' 'Equipamento         ' l_cabec.
  ENDIF.

* Identificação Técnica
  IF ( p_equnr-low IS NOT INITIAL ) AND ( p_equnr-high IS NOT INITIAL ).
    CONCATENATE p_tidnr-low 'a' p_tidnr-high INTO l_cabec SEPARATED BY space.
  ELSEIF ( p_tidnr-low IS NOT INITIAL ).
    l_cabec = p_tidnr-low.

  ELSE.
    l_cabec = ' '.
  ENDIF.

  IF p_tidnr IS NOT INITIAL.
    mc_preenche_cabec:  'S' 'Identifição Técnica ' l_cabec.
  ENDIF.

* Periodo de seleção
  WRITE: p_gstrp-low TO l_cabec1,
         p_gstrp-high TO l_cabec2.
  CONCATENATE l_cabec1 'a' l_cabec2 INTO l_cabec SEPARATED BY space.
  IF p_qmart IS INITIAL.
    mc_preenche_cabec:  'S' 'Data Planej.Ordem   ' l_cabec.
  ELSE.
    mc_preenche_cabec:  'S' 'Data Planej.Nota   ' l_cabec.
  ENDIF.

ENDFORM.                    " F_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM f_catalogo .

  IF p_auart IS NOT INITIAL.

    mc_preenche_fieldcat:
  "                                                          Tam  Sum ID  NO_ZERO
    'WERKS'            'CHAR' 'Centro'                       '25' '' '' ' ' '',
    'TPLNR   '         'CHAR' 'Local Instalação'             '40' '' '' ' ' '',
    'PLTXT   '         'CHAR' 'Desc Local'                   '40' '' '' ' ' '',
    'EQUNRDES'         'CHAR' 'Equipamento'                  '40' '' '' ' ' '',
    'TIDNR'            'CHAR' 'Ident. Técnica'               '10' '' '' ' ' '',
    'WARPL'            'CHAR' 'Plano'                        '15' '' '' 'X' '',
    'WPTXT'            'CHAR' 'Descrição'                    '30' '' '' ' ' '',
    'BAUTL'            'CHAR' 'Sistema'                      '03' '' '' ' ' '',
    'MAKTX'            'CHAR' 'Desc.Sistema'                 '40' '' '' ' ' '',
    'AUFNR'            'CHAR' 'Ordem'                        '15' '' '' 'X' '',
    'AUART'            'CHAR' 'Tipo Ordem'                   '08' '' '' ' ' '',
    'NPLDA'            'DATS' 'Data Planej. Plano'           '15' '' '' ' ' '',
    'GSTRP'            'DATS' 'Data Planej. Ordem'           '15' '' '' ' ' '',
    'ERDAT'            'DATS' 'Data Criação Ordem'           '16' '' '' ' ' '',
    'IDAT1'            'DATS' 'Data Liberação Ordem'         '18' '' '' ' ' ''.

  ELSE.

    mc_preenche_fieldcat:
   'IWERK'            'CHAR' 'Centro'                       '25' '' '' ' ' ' ',
   'TPLNR'            'CHAR' 'Local Inst'                   '25' '' '' ' ' ' ',
   'EQUNR'            'CHAR' 'Equipamento'                  '40' '' '' 'X' 'X',
   'WARPL'            'CHAR' 'Plano'                        '15' '' '' 'X' 'X',
   'WPTXT'            'CHAR' 'Descrição'                    '30' '' '' ' ' ' ',
   'QMART'            'CHAR' 'Tipo nota'                    '08' '' '' ' ' ' ',
   'QMNUM'            'CHAR' 'Nota     '                    '08' '' '' 'X' 'X',
   'NPLDA'            'DATS' 'Data Planej. Plano'           '15' '' '' ' ' ' ',
   'QMDAT'            'DATS' 'Data Planejamento da nota'    '15' '' '' ' ' ' ',
   'ERDAT'            'DATS' 'Data Criação da nota '        '18' '' '' ' ' ' '.

  ENDIF.
ENDFORM.                    " F_CATALOGO
*&---------------------------------------------------------------------*
*&      Form  F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_classificacao .

  CLEAR vg_i.
  REFRESH it_sort.

*  mc_preenche_class:
*                     'WERKS'       'X' 'X' 'X',
*                     'TKNUM'       'X' 'X' 'X'.
ENDFORM.                    " F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout .

  st_layout-zebra               = 'X'.
  st_layout-group_buttons       = ' '.
  st_layout-totals_only         = ' '.

  it_layout_alv-box_fieldname = 'MARK'.

* Otimizar colunas na tela
  st_layout-colwidth_optimize   = 'X'.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      it_fieldcat       = it_fieldcat
      is_layout         = st_layout
    IMPORTING
      et_fieldcat       = it_fieldcat_alv
      es_layout         = it_layout_alv
      et_special_groups = it_special_groups.

ENDFORM.                    " F_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eventos .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = i_events.

  READ TABLE i_events
       WITH KEY name = slis_ev_top_of_page
       INTO w_events.
  IF sy-subrc = 0.
    MOVE 'ENCABEZADO' TO w_events-form.
    MODIFY i_events FROM w_events INDEX sy-tabix.
  ENDIF.


ENDFORM.                    " EVENTOS



*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_relatorio TABLES pt_outtab TYPE table.

* Não otimizar colunas no preview e nem na impressão
  st_grid_settings-no_colwopt = cc_x.

  it_layout_alv-box_fieldname = 'MARK'.
  it_layout_alv-box_tabname = 'PT_OUTTAB'.

* Não imprimir o relatório estatístico antes do relatório
  st_print-no_print_selinfos  = cc_x.
  st_print-no_print_listinfos = cc_x.

  v_repid = sy-repid.
  CALL FUNCTION 'K_KKB_SAVE_MODE_GET'
    IMPORTING
      e_save = v_save.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_repid
      is_layout                = it_layout_alv
      it_fieldcat              = it_fieldcat_alv
      it_special_groups        = it_special_groups
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_grid_settings          = st_grid_settings
      it_sort                  = it_sort[]
      i_default                = cc_x
      is_print                 = st_print
      i_save                   = v_save
      is_variant               = variant
    TABLES
      t_outtab                 = pt_outtab  " t_relatorio "
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE cc_i NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

ENDFORM.                    "f_relatorio
*&---------------------------------------------------------------------*
*&      Form  F_F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_f4_variant .

  DATA: variant_exit(1) TYPE c.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = variant
      i_save     = 'A'
    IMPORTING
      e_exit     = variant_exit
      es_variant = def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc EQ 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF variant_exit = space.
      p_vari = def_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_F4_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_variant .

  IF NOT p_vari IS INITIAL.

    MOVE variant TO def_variant.
    MOVE p_vari TO def_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = def_variant.

    variant = def_variant.

  ELSE.
    CLEAR variant.
    variant-report = v_repid.
  ENDIF.

ENDFORM.                    " F_VARIANT

*ENDFORM.                    " DESMARCA_TODOS_REGISTROS
*&---------------------------------------------------------------------*
*&      Form  PREPARA_SELECIONADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepara_selecionados .

  DATA: v_qtde  TYPE p DECIMALS 2,
        v_tabix TYPE p DECIMALS 2.

  DESCRIBE TABLE t_relatorio LINES v_qtde.

  CLEAR t_zpmr0008i.
  REFRESH t_zpmr0008i.

  LOOP AT t_relatorio INTO w_relatorio WHERE mark EQ abap_true.

    v_tabix = ( sy-tabix / v_qtde ) * 100 .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = v_tabix
        text       = 'Aguarde...'.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_relatorio-aufnr
      IMPORTING
        output = w_relatorio-aufnr.

    TRY .
        t_viaufks = t_viaufks[ aufnr = w_relatorio-aufnr ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR t_viaufks.
    ENDTRY.

    LOOP AT t_afvc WHERE aufpl = t_viaufks-aufpl.

      TRY .
          t_iflo = t_iflo[ tplnr = t_viaufks-tplnr ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR t_iflo.
      ENDTRY.

      APPEND VALUE #(
                      bukrs = w_relatorio-bukrs
                      werks = w_relatorio-werks
                      equnr = w_relatorio-equnr
                      auart = w_relatorio-auart
                      aufnr = w_relatorio-aufnr
                      tidnr = w_relatorio-tidnr
                      erdat = w_relatorio-erdat
                      idat1 = w_relatorio-idat1
                      warpl = w_relatorio-warpl
                      wptxt = w_relatorio-wptxt
                      bautl = w_relatorio-bautl
                      maktx = w_relatorio-maktx
                      aufpl = t_afvc-aufpl
                      tplnr = w_relatorio-tplnr
                      pltxt = w_relatorio-pltxt
                      gltrp = t_caufv-gltrp
                      aplzl = |{ t_afvc-aplzl ALPHA = OUT }|
                      vornr = t_afvc-vornr
                      ltxa1 = t_afvc-ltxa1
                      ltxa2 = t_afvc-ltxa2
                      equnrdes  = w_relatorio-equnrdes
                     ) TO t_zpmr0008i.

      IF NOT line_exists( t_zpmr0008c[ bukrs = w_relatorio-bukrs
                                       werks = w_relatorio-werks
                                       equnr = w_relatorio-equnr
                                       auart = w_relatorio-auart
                                       warpl = w_relatorio-warpl
                                     ] ).

        APPEND VALUE #(
                        bukrs      = w_relatorio-bukrs
                        werks      = w_relatorio-werks
                        equnr      = w_relatorio-equnr
                        auart      = w_relatorio-auart
                        aufnr      = w_relatorio-aufnr "Incluir inf ordem "AOENNING
                        tidnr      = abap_false
                        erdat      = sy-datum
                        idat1      = sy-datum
                        warpl      = w_relatorio-warpl "Incluir inf plano            "AOENNING
                        wptxt      = w_relatorio-wptxt "Incluir inf desc plano       "AOENNING
                        bautl      = w_relatorio-bautl
                        maktx      = w_relatorio-maktx
                        aufpl      = '0'
                        aplzl      = '0'
                        ltxa1      = abap_false
                        ltxa2      = abap_false
                        equnrdes   = w_relatorio-equnrdes
                        tplnr      = w_relatorio-tplnr
                        pltxt      = w_relatorio-pltxt
                        gltrp      = t_caufv-gltrp
                       ) TO t_zpmr0008c.

      ENDIF.

      IF NOT line_exists( t_zpmr0008c2[ bukrs = w_relatorio-bukrs
                                        werks = w_relatorio-werks
                                        equnr = w_relatorio-equnr
                                      ] ).

        APPEND VALUE #(
                        bukrs     = w_relatorio-bukrs
                        werks     = w_relatorio-werks
                        equnr     = w_relatorio-equnr
                        auart     = w_relatorio-auart
                        aufnr     = w_relatorio-aufnr
                        tidnr     = w_relatorio-tidnr
                        erdat     = w_relatorio-erdat
                        gstrp     = w_relatorio-gstrp
                        idat1     = w_relatorio-idat1
                        warpl     = w_relatorio-warpl
                        wptxt     = w_relatorio-wptxt
                        bautl     = w_relatorio-bautl
                        maktx     = w_relatorio-maktx
                        aufpl     = '0'
                        aplzl     = '0'
                        ltxa1     = abap_false
                        ltxa2     = abap_false
                        equnrdes  = w_relatorio-equnrdes
                        tplnr     = w_relatorio-tplnr
                        pltxt     = w_relatorio-pltxt
                        gltrp     = t_caufv-gltrp
                       ) TO t_zpmr0008c2.

      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF p_plan IS NOT INITIAL.
    SORT t_zpmr0008c2 BY warpl.
    SORT t_zpmr0008c  BY warpl.
    DELETE ADJACENT DUPLICATES FROM t_zpmr0008c2 COMPARING warpl.
    DELETE ADJACENT DUPLICATES FROM t_zpmr0008c  COMPARING warpl.
  ENDIF.

  break abap.
ENDFORM.                    " PREPARA_SELECIONADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_SELECIONADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_selecionados .


*  ----------------------------------------------------------------------*
*   Variaveis
*  ----------------------------------------------------------------------*
  DATA: w_fm_name TYPE rs38l_fnam.
*  ----------------------------------------------------------------------*
*   Constantes
*  ----------------------------------------------------------------------*
  DATA: c_formname TYPE tdsfname VALUE 'ZPMR00082'.

  DATA: ls_t001k TYPE  t001k,
        s_swerk  TYPE swerk.


  CLEAR:   ls_t001k.
  CALL FUNCTION 'AIP01_PLANT_DETERMINE'
    EXPORTING
      i_werks  = w_t001w-werks
    IMPORTING
      es_t001k = ls_t001k
    EXCEPTIONS
      OTHERS   = 1.


  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_PM_IW32'
        AND valfrom EQ @ls_t001k-bukrs.


  IF i_data IS NOT INITIAL.
    c_formname = 'ZPMF0011'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = c_formname
    IMPORTING
      fm_name            = w_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    WRITE: / 'ERROR 1'.
  ENDIF.

  CALL FUNCTION w_fm_name
    EXPORTING
      w_t001       = w_t001
      w_t001w      = w_t001w
      tordem       = p_plan
    TABLES
      t_zpmr0008c  = t_zpmr0008c
      t_zpmr0008c2 = t_zpmr0008c2
      t_zpmr0008i  = t_zpmr0008i.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " IMPRIME_SELECIONADOS

*----------------------------------------------------------------------*
*      -->P_MSG1     text
*      -->P_MSG2     text
*----------------------------------------------------------------------*
FORM f_lupa USING p_msg1 p_msg2.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

  CONCATENATE p_msg1 p_msg2 INTO vl_message SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.
ENDFORM. "f_lupa
*&---------------------------------------------------------------------*
*&      Form  SELEC_DADOS_NOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_dados_not.

  REFRESH: t_caufv, t_mpla, t_equi, t_t001w, t_relatorio, t_afvc, t_jest, t_mhis, t_zpmr0008c, t_zpmr0008c2, t_zpmr0008i, t_makt, t_viqmel, t_carac, t_local.
  CLEAR: v_permissao.

  PERFORM f_lupa USING 'Selecionando nota de manutenção' space.

* Busca tipo nota de manutenção.
  SELECT equnr iwerk erdat qmnum qmtxt qmart bautl warpl tplnr beber objnr bukrs qmdat
  INTO CORRESPONDING FIELDS OF TABLE t_viqmel
    FROM viqmel
    WHERE qmart IN p_qmart
    AND tplnr IN p_tplnr
    AND iwerk IN p_werks
    AND erdat IN p_auart
    AND equnr IN p_equnr.

  PERFORM f_lupa USING 'Selecionando equipamento(s) das notas de manutenção' space.

* Busca equipamento das notas de manutenção
  SELECT viqmel~qmnum equi~eqart equi~equnr equi~herst equi~typbz
    INTO CORRESPONDING FIELDS OF TABLE t_equi
      FROM  viqmel
        INNER JOIN equi ON equi~equnr = viqmel~equnr
        FOR ALL ENTRIES  IN t_viqmel
        WHERE qmnum      EQ t_viqmel-qmnum
         AND  viqmel~equnr IN p_equnr
         AND  equi~eqart IN p_eqart.

  CHECK t_viqmel[] IS NOT INITIAL.

*  Busca os status
  SELECT jest~objnr jest~stat tj02t~txt04
  INTO TABLE t_jest
      FROM jest
        INNER JOIN tj02t ON tj02t~istat = jest~stat
        FOR ALL ENTRIES IN t_viqmel
        WHERE jest~objnr  EQ t_viqmel-objnr
          AND jest~inact  EQ abap_true   " Somente os ativos
          AND tj02t~spras EQ sy-langu.

*    Deleta os status que não forem o de concluído.
  DELETE t_jest WHERE txt04 NE 'MSEN'.

*  Busca planos de Manutenção
  SELECT mpla~warpl mpos~qmnum mpla~ersdt mpla~wptxt
    INTO TABLE t_mpla
     FROM mpla
      INNER JOIN mhis ON mhis~warpl = mpla~warpl
      INNER JOIN mpos ON mpos~warpl = mpla~warpl
      FOR ALL ENTRIES IN t_viqmel
       WHERE mpos~qmnum EQ t_viqmel-qmnum.

  SELECT werks name1 iwerk
   INTO TABLE t_t001w
     FROM  t001w
     FOR ALL ENTRIES IN t_viqmel
     WHERE iwerk EQ t_viqmel-iwerk.

* BUSCA DATA DE PLANEJAMENTO DO PLANO
  SELECT viqmel~qmnum mhis~warpl mhis~abnum mhis~nplda mhis~horda
  INTO TABLE t_mhis
    FROM viqmel
    INNER JOIN mhis ON  mhis~warpl = viqmel~warpl
      FOR ALL ENTRIES IN t_viqmel
      WHERE objnr EQ t_viqmel-objnr
      AND nplda >= sy-datum.

  SORT t_mhis BY nplda ASCENDING.

* Delete as notas que não forem de planos
  LOOP AT t_viqmel.
    v_tabix = sy-tabix.

*   Delete as notas que não forem do plano de manutenção
    IF NOT line_exists( t_mpla[ warpl = t_viqmel-warpl ] ).
      DELETE t_viqmel INDEX v_tabix. CONTINUE.
    ENDIF.

    PERFORM f_lupa USING 'Eliminando ordens encerradas' space.

*   Deleta as notas encerradas.
    IF line_exists( t_jest[ objnr = t_viqmel-objnr ] ).
      DELETE t_viqmel INDEX v_tabix. CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_SELECIONADOS_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_selecionados_notas .

  DATA: qmnum TYPE RANGE OF viqmel-qmnum WITH HEADER LINE.
  DATA: mptyp TYPE RANGE OF imptt-mptyp WITH HEADER LINE.

  LOOP AT t_relatorio INTO DATA(rel) WHERE mark EQ abap_true.
    APPEND VALUE #(
                    option = 'EQ'
                    sign = 'I'
                    low = |{ rel-qmnum ALPHA = IN }|
                  ) TO qmnum.
  ENDLOOP.

  DATA: saida    TYPE TABLE OF ztpm0001 WITH HEADER LINE.

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam,
        ls_t001k    TYPE  t001k.

  SELECT vi~iwerk t0~name1 vi~tplnr if~pltxt vi~equnr
         eq~eqktx vi~warpl mp~wptxt vi~qmnum im~point im~psort ct~atinn ct~atbez im~mptyp im~pttxt
        FROM viqmel AS vi
    INNER JOIN t001w AS t0 ON t0~werks EQ vi~iwerk
    INNER JOIN iflo AS if ON if~tplnr EQ vi~tplnr
    INNER JOIN eqkt AS eq ON eq~equnr EQ vi~equnr
    INNER JOIN mpla AS mp ON mp~warpl EQ vi~warpl
    INNER JOIN equi AS eu ON eu~equnr EQ vi~equnr
    INNER JOIN imptt AS im ON im~mpobj EQ eu~objnr
    INNER JOIN cabnt AS ct ON ct~atinn EQ im~atinn
    INTO TABLE saida
    WHERE vi~qmnum IN qmnum
      AND im~inact EQ abap_false
      AND ct~spras EQ 'PT'.

  SORT saida ASCENDING BY point.

  IF NOT line_exists( saida[ mptyp = 'P' ] ).
    MESSAGE 'Existe Linha com categoria não cadastrada ou Incorreta!.' TYPE 'I'.
    EXIT.
  ENDIF.

  CHECK NOT saida[] IS INITIAL.

  SORT saida BY qmnum psort.


  "verificar se ordem pertence a empresa tgg.
  CLEAR: ls_t001k.
  CALL FUNCTION 'AIP01_PLANT_DETERMINE'
    EXPORTING
      i_werks  = p_werks-low
    IMPORTING
      es_t001k = ls_t001k
    EXCEPTIONS
      OTHERS   = 1.


  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_PM_IW32'
        AND valfrom EQ @ls_t001k-bukrs.


  IF i_data IS INITIAL.
    vl_formname = 'ZPMF0001'.
  ELSE.
    vl_formname = 'ZPMF0014'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CALL FUNCTION vl_name
    TABLES
      it_equi          = saida
      it_qmnum         = saida
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
