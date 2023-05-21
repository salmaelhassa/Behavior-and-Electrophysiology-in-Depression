% Concatenate the matrices
res4excl = [PRENEG; PREPOS; POSTNEG; POSTPOS];

% Create cell array of matrix names
matrix_names = {'PRENEG'; 'PREPOS'; 'POSTNEG'; 'POSTPOS'};

% Repeat each matrix name 90 times to create the label column
label_column = repelem(matrix_names, 90);

% Create a cell array with the matrix names and corresponding values
matrix_data = [repmat({'PRENEG'}, 90, 1), num2cell(PRENEG);
               repmat({'PREPOS'}, 90, 1), num2cell(PREPOS);
               repmat({'POSTNEG'}, 90, 1), num2cell(POSTNEG);
               repmat({'POSTPOS'}, 90, 1), num2cell(POSTPOS)];

% Create a table from the matrix with variable names
column_names = {'ValencebyDBS', 'Gamma'};
T = array2table(matrix_data, 'VariableNames', column_names);

% Specify the Excel file name and sheet name
filename = ['/Users/bijankilab-ra4/Documents/MATLAB/ECoG/neuralData/' subj '/DBSTRD00' subj(end) '_EPHYS.xlsx'];

% Write the matrix to an Excel file using writetable
writetable(T, filename);
