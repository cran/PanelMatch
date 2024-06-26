# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

get_vit_index <- function(t_id_key, control_treatment_t_ids, control_treatment_set_nums) {
    .Call('_PanelMatch_get_vit_index', PACKAGE = 'PanelMatch', t_id_key, control_treatment_t_ids, control_treatment_set_nums)
}

get_dits <- function(t_id_key, nonempty_t_ids) {
    .Call('_PanelMatch_get_dits', PACKAGE = 'PanelMatch', t_id_key, nonempty_t_ids)
}

prep_lead_years <- function(ts, lead_window) {
    .Call('_PanelMatch_prep_lead_years', PACKAGE = 'PanelMatch', ts, lead_window)
}

needs_adjustment <- function(set_index_list) {
    .Call('_PanelMatch_needs_adjustment', PACKAGE = 'PanelMatch', set_index_list)
}

equality_four_cpp <- function(Wit_vals, y, z) {
    .Call('_PanelMatch_equality_four_cpp', PACKAGE = 'PanelMatch', Wit_vals, y, z)
}

get_vit_index_unsigned <- function(t_id_key, control_treatment_t_ids, control_treatment_set_nums) {
    .Call('_PanelMatch_get_vit_index_unsigned', PACKAGE = 'PanelMatch', t_id_key, control_treatment_t_ids, control_treatment_set_nums)
}

handle_vits <- function(nrow_data, mset_size, num_empty, weights, tidkey, control_treatment_tids, ct_set_nums) {
    .Call('_PanelMatch_handle_vits', PACKAGE = 'PanelMatch', nrow_data, mset_size, num_empty, weights, tidkey, control_treatment_tids, ct_set_nums)
}

get_yearly_dmats <- function(expanded_data, treated_ids, ts_to_fetch, matched_sets, lag) {
    .Call('_PanelMatch_get_yearly_dmats', PACKAGE = 'PanelMatch', expanded_data, treated_ids, ts_to_fetch, matched_sets, lag)
}

check_treated_units_for_treatment_reversion <- function(compmat, compmat_row_units, compmat_cols, lead, treated_ids, treated_ts) {
    .Call('_PanelMatch_check_treated_units_for_treatment_reversion', PACKAGE = 'PanelMatch', compmat, compmat_row_units, compmat_cols, lead, treated_ids, treated_ts)
}

check_control_units_for_treatment_restriction <- function(compmat, compmat_row_units, compmat_cols, lead, sets, control_start_years) {
    .Call('_PanelMatch_check_control_units_for_treatment_restriction', PACKAGE = 'PanelMatch', compmat, compmat_row_units, compmat_cols, lead, sets, control_start_years)
}

do_exact_matching_refinement <- function(balanced_data, lag, row_key, control_data, treatment_data, exact_match_variable_column_index) {
    .Call('_PanelMatch_do_exact_matching_refinement', PACKAGE = 'PanelMatch', balanced_data, lag, row_key, control_data, treatment_data, exact_match_variable_column_index)
}

check_missing_data_treated_units <- function(subset_data, sets, tid_pairs, treated_tid_pairs, treated_ids, lead) {
    .Call('_PanelMatch_check_missing_data_treated_units', PACKAGE = 'PanelMatch', subset_data, sets, tid_pairs, treated_tid_pairs, treated_ids, lead)
}

check_missing_data_control_units <- function(subset_data, sets, prepared_sets, tid_pairs, lead) {
    .Call('_PanelMatch_check_missing_data_control_units', PACKAGE = 'PanelMatch', subset_data, sets, prepared_sets, tid_pairs, lead)
}

enforce_strict_histories <- function(control_histories, strict_period) {
    .Call('_PanelMatch_enforce_strict_histories', PACKAGE = 'PanelMatch', control_histories, strict_period)
}

get_treated_indices <- function(ordered_df, treated_indices, treat_col_idx, unit_var_col) {
    .Call('_PanelMatch_get_treated_indices', PACKAGE = 'PanelMatch', ordered_df, treated_indices, treat_col_idx, unit_var_col)
}

get_comparison_histories <- function(compmat, ts, ids, t_col, id_col, L, treat_col, atc) {
    .Call('_PanelMatch_get_comparison_histories', PACKAGE = 'PanelMatch', compmat, ts, ids, t_col, id_col, L, treat_col, atc)
}

get_msets_helper <- function(control_history_list, widemat, t_as_col_nums, ids, L) {
    .Call('_PanelMatch_get_msets_helper', PACKAGE = 'PanelMatch', control_history_list, widemat, t_as_col_nums, ids, L)
}

non_matching_matcher <- function(control_history_list, widemat, t_as_col_nums, ids, L, missing_window) {
    .Call('_PanelMatch_non_matching_matcher', PACKAGE = 'PanelMatch', control_history_list, widemat, t_as_col_nums, ids, L, missing_window)
}

filter_placebo_results <- function(expanded_data, ordered_outcome_data, treated_ids, treated_ts, sets, lag) {
    .Call('_PanelMatch_filter_placebo_results', PACKAGE = 'PanelMatch', expanded_data, ordered_outcome_data, treated_ids, treated_ts, sets, lag)
}

