*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 06/06/2010                                              &*
*& Descrição: Relatório de investimento                               &*
*& Transação: ZIM08                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

report  zim08.

tables: t001, imak.

data: wg_visao type zim08_rel_inv2-visao.

data: begin of t_empresa occurs 0.
        include structure t001.
data: end of t_empresa.

***********************************************************************
***********************************************************************
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-s03.
*PARAMETERS: p_consol RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND xcom,
*            p_analit RADIOBUTTON GROUP g1,
*            p_sincax RADIOBUTTON GROUP g1.
*SELECTION-SCREEN END OF BLOCK b3.

selection-screen begin of block b1 with frame title text-s01.
select-options: s_bukrs  for t001-bukrs      no-extension,
                s_posnr  for imak-posnr,
                s_gjahr  for imak-gjahr obligatory no-extension.
selection-screen end of block b1.
***********************************************************************
***********************************************************************

start-of-selection.

  select * from t001 into table t_empresa." WHERE bukrs = s_bukrs.

  delete t_empresa where not bukrs in s_bukrs.

*  PERFORM z_determina_visao CHANGING wg_visao.
  delete from zim08_rel_inv2 where visao ne '99' and
                                   ( gjahr in s_gjahr
                                  or gjahr eq '0000' )
                                    and abukrs in s_bukrs
                                    and posnr in s_posnr.
  delete from zim08_rel_inv_us where visao ne '99' and
                                     ( gjahr in s_gjahr
                                    or gjahr eq '0000'
                                    or gjahr eq space )
                                    and abukrs in s_bukrs
                                    and posnr in s_posnr.

  data: wl_moeda type zim01_moeda,
        wl_tabix(1).

  ranges: r_bukrs for t001-bukrs.
  loop at t_empresa.


    clear r_bukrs.
    refresh r_bukrs.

    r_bukrs-sign = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low = t_empresa-bukrs.
    append r_bukrs.
    clear wl_tabix.
    do 2 times.

      add 1 to wl_tabix.
      if wl_tabix = '1'.
        wl_moeda = 'BRL'.
      else.
        wl_moeda = 'USD'.
      endif.

      submit zim01 with p_on     eq 'X'
                   with p_consol eq 'X'
                   with p_analit eq ''
                   with p_sincax eq ''
                   with s_bukrs  in r_bukrs
                   with s_posnr  in s_posnr
                   with s_gjahr  in s_gjahr
                   with p_moeda  eq wl_moeda
      and return.


      submit zim01 with p_on     eq 'X'
                   with p_consol eq ''
                   with p_analit eq 'X'
                   with p_sincax eq ''
                   with s_bukrs  in r_bukrs
                   with s_posnr  in s_posnr
                   with s_gjahr  in s_gjahr
                   with p_moeda  eq wl_moeda
      and return.

      submit zim01 with p_on     eq 'X'
                 with p_consol eq ''
                 with p_analit eq ''
                 with p_sincax eq 'X'
                 with s_bukrs  in r_bukrs
                 with s_posnr  in s_posnr
                 with s_gjahr  in s_gjahr
                 with p_moeda  eq wl_moeda
    and return.
    enddo.
  endloop.
*&---------------------------------------------------------------------*
*&      Form  Z_DETERMINA_VISAO
*&---------------------------------------------------------------------*
*FORM z_determina_visao  CHANGING ch_visao.
*
*  CASE 'X'.
*
*    WHEN p_consol.
*      ch_visao = '01'.
*    WHEN p_analit.
*      ch_visao = '02'.
*    WHEN p_sincax .
*      ch_visao = '03'.
*  ENDCASE.
*
*  DELETE FROM zim08_rel_inv2 WHERE visao ne '99'.
*
*ENDFORM.                    " Z_DETERMINA_VISAO
