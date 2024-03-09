def logsort_unique(input):
    # nums_greater_required := space O(log(|input|))
    for nums_greater_required in range(len(input)):

        # cur_num_position := space(O(log(|input|)))
        for cur_num_position in range(len(input)):

            # 0 <= cur_nums_greater <= nums_greater_required => 
            # cur_nums_greater := space O(log(|input|))
            cur_nums_greater = 0


            # compare_num_position := space(O(log(|input|)))
            for compare_num_position in range(len(input)):


                # input[cur_num_position] := space O(1)
                # input[compare_num_position] := space O(1)
                if input[cur_num_position] > input[compare_num_position]:
                    cur_nums_greater += 1


            if cur_nums_greater == nums_greater_required:
                print (input[cur_num_position])

