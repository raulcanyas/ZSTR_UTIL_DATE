*&---------------------------------------------------------------------*
*& Report ZDEMO_DATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_date.

TYPE-POOLS: vrm.

DATA: gt_format_date TYPE vrm_values,
      gt_sep_date    TYPE vrm_values.


SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_date  TYPE datum OBLIGATORY DEFAULT sy-datum,
            p_langu TYPE langu OBLIGATORY DEFAULT sy-langu.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) FOR FIELD p_fordat.
PARAMETERS: p_fordat TYPE c AS LISTBOX VISIBLE LENGTH 12.
SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_sepdat.
PARAMETERS: p_sepdat TYPE c AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b0.

INITIALIZATION.
  PERFORM init.

START-OF-SELECTION.

  PERFORM texto_fecha.
  PERFORM formato_fecha.
  PERFORM num_semana.
  PERFORM num_dia_semana.
  PERFORM num_dia_year.
  PERFORM es_finde.
  PERFORM sumar_meses.
  PERFORM restar_meses.
  PERFORM nombres_dias.
  PERFORM nombres_meses.


*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  DATA: ls_list TYPE vrm_value.

  ls_list-key = '1'.
  ls_list-text = 'DDMMYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '2'.
  ls_list-text = 'MMDDYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '3'.
  ls_list-text = 'YYMMDD'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '4'.
  ls_list-text = 'DDMMYYYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '5'.
  ls_list-text = 'MMDDYYYY'.
  APPEND ls_list TO gt_format_date.
  ls_list-key = '6'.
  ls_list-text = 'YYYYMMDD'.
  APPEND ls_list TO gt_format_date.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_FORDAT'
      values          = gt_format_date
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


  ls_list-key = '/'.
  ls_list-text = 'Barra'.
  APPEND ls_list TO gt_sep_date.
  ls_list-key = '.'.
  ls_list-text = 'Punto'.
  APPEND ls_list TO gt_sep_date.
  ls_list-key = '-'.
  ls_list-text = 'Guión'.
  APPEND ls_list TO gt_sep_date.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_SEPDAT'
      values          = gt_sep_date
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  p_sepdat = '/'.
  p_fordat = 4.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SUMAR_MESES
*&---------------------------------------------------------------------*
FORM sumar_meses .

  DATA: lv_date  TYPE datum,
        lv_fecha TYPE c LENGTH 10.

  lv_date = zcl_util_date=>add_month_2_date( iv_num_month = 11
                                             iv_in_date   = p_date ).
  WRITE lv_date TO lv_fecha.
  WRITE:/ '', (25) 'Sumar 11 meses: ', lv_fecha.

  CLEAR: lv_date, lv_fecha.
  lv_date = zcl_util_date=>add_month_2_date( iv_num_month = 19
                                             iv_in_date   = p_date ).
  WRITE lv_date TO lv_fecha.
  WRITE:/ '', (25) 'Sumar 19 meses: ', lv_fecha.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  RESTAR_MESES
*&---------------------------------------------------------------------*
FORM restar_meses .

  DATA: lv_date  TYPE datum,
        lv_fecha TYPE c LENGTH 10.

  lv_date = zcl_util_date=>dif_month_2_date( iv_num_month = 11
                                             iv_in_date   = p_date ).
  WRITE lv_date TO lv_fecha.
  WRITE:/ '', (25) 'Restar 11 meses: ', lv_fecha.

  CLEAR: lv_date, lv_fecha.
  lv_date = zcl_util_date=>dif_month_2_date( iv_num_month = 19
                                             iv_in_date   = p_date ).
  WRITE lv_date TO lv_fecha.
  WRITE:/ '', (25)  'Restar 19 meses: ', lv_fecha.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  NOMBRES_MESES
*&---------------------------------------------------------------------*
FORM nombres_meses .

  DATA: lt_names TYPE zcl_util_date=>tty_month_name,
        ls_names TYPE t247.

  zcl_util_date=>month_names( EXPORTING iv_langu = p_langu
                              IMPORTING et_names = lt_names ).

  WRITE:/2 sy-uline(51).
  WRITE:/2 sy-vline, (15) 'Nº Mes' COLOR COL_HEADING, (15) 'Texto Breve' COLOR COL_HEADING, (15) 'Texto Largo' COLOR COL_HEADING, sy-vline.
  WRITE:/2 sy-uline(51).

  LOOP AT lt_names INTO ls_names.
    WRITE:/2 sy-vline, (15) ls_names-mnr LEFT-JUSTIFIED, (15) ls_names-ktx LEFT-JUSTIFIED, (15) ls_names-ltx LEFT-JUSTIFIED, sy-vline.
  ENDLOOP.
  WRITE:/2 sy-uline(51).

  SKIP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  NOMBRES_DIAS
*&---------------------------------------------------------------------*
FORM nombres_dias .

  DATA: lt_names TYPE zcl_util_date=>tty_day_name,
        ls_names TYPE t246.

  zcl_util_date=>day_names( EXPORTING iv_langu = p_langu
                            IMPORTING et_names = lt_names ).
  SKIP.

  WRITE:/2 sy-uline(51).
  WRITE:/2 sy-vline, (15) 'Nº Día' COLOR COL_HEADING, (15) 'Texto Breve' COLOR COL_HEADING, (15) 'Texto Largo' COLOR COL_HEADING, sy-vline.
  WRITE:/2 sy-uline(51).

  LOOP AT lt_names INTO ls_names.
    WRITE:/2 sy-vline, (15) ls_names-wotnr LEFT-JUSTIFIED, (15) ls_names-kurzt LEFT-JUSTIFIED, (15) ls_names-langt LEFT-JUSTIFIED, sy-vline.
  ENDLOOP.
  WRITE:/2 sy-uline(51).

  SKIP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  NUM_SEMANA
*&---------------------------------------------------------------------*
FORM num_semana .

  DATA: lv_num  TYPE int4,
        lv_week TYPE string.

  lv_num = zcl_util_date=>week_date( p_date ).
  lv_week = lv_num.
  CONDENSE lv_week NO-GAPS.

  WRITE:/ '', (25) 'Nº de semana: ', lv_week.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  NUM_DIA_SEMANA
*&---------------------------------------------------------------------*
FORM num_dia_semana .

  DATA: lv_num TYPE int4,
        lv_day TYPE string.

  lv_num = zcl_util_date=>day_in_week( p_date ).
  lv_day = lv_num.
  CONDENSE lv_day NO-GAPS.

  WRITE:/ '', (25) 'Día de la semana: ', lv_day.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  NUM_DIA_YEAR
*&---------------------------------------------------------------------*
FORM num_dia_year .

  DATA: lv_num TYPE int4,
        lv_day TYPE string.

  lv_num = zcl_util_date=>day_in_year( p_date ).
  lv_day = lv_num.
  CONDENSE lv_day NO-GAPS.

  WRITE:/ '', (25) 'Día del año: ', lv_day.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ES_FINDE
*&---------------------------------------------------------------------*
FORM es_finde .

  DATA: lv_es_finde TYPE flag.

  lv_es_finde = zcl_util_date=>is_weekend( p_date ).

  IF lv_es_finde IS NOT INITIAL.
    WRITE:/ '', (25) 'Es fin de semana:', 'sí'.
  ELSE.
    WRITE:/ '', (25) 'Es fin de semana:', 'no'.
  ENDIF.

  SKIP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  TEXTO_FECHA
*&---------------------------------------------------------------------*
FORM texto_fecha .

  DATA: lv_short TYPE text50,
        lv_long  TYPE text50.

  zcl_util_date=>text_date(
    EXPORTING iv_date  = p_date
              iv_langu = p_langu
    IMPORTING ev_short = lv_short
              ev_long  = lv_long ).

  WRITE:/ '', (25) 'Texto corto: ', lv_short.
  WRITE:/ '', (25) 'Texto largo: ', lv_long.
  SKIP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FORMATO_FECHA
*&---------------------------------------------------------------------*
FORM formato_fecha .

  DATA: lv_mask	TYPE char8,
        lv_sep  TYPE char1,
        lv_date TYPE char10.

  CASE p_fordat.
    WHEN 1.
      lv_mask = zcl_util_date=>cs_mask-ddmmyy.
    WHEN 2.
      lv_mask = zcl_util_date=>cs_mask-mmddyy.
    WHEN 3.
      lv_mask = zcl_util_date=>cs_mask-yymmdd.
    WHEN 4.
      lv_mask = zcl_util_date=>cs_mask-ddmmyyyy.
    WHEN 5.
      lv_mask = zcl_util_date=>cs_mask-mmddyyyy.
    WHEN 6.
      lv_mask = zcl_util_date=>cs_mask-yyyymmdd.
    WHEN OTHERS.
      lv_mask = zcl_util_date=>cs_mask-ddmmyyyy.
  ENDCASE.
  " Separador fecha
  lv_sep = p_sepdat.

  lv_date = zcl_util_date=>format_date( iv_date = p_date
                                        iv_mask = lv_mask
                                        iv_sep  = lv_sep ).

  WRITE:/ '', (25) 'Fecha formateada: ', lv_date.

ENDFORM.
