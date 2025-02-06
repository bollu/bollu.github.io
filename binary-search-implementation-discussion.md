int left = range_left - 1, right = range_right + 1;
while(right - left > 1) {
  int mid = (left + right) / 2;
  if (as[mid] >= val) {
    right = mid;
  } else {
    left = mid
  }
}

[
12:15 AM
]
Shaanjeet
:
for your second point, i wouldnt treat it as < or <=. I treat it as if the condition is satisfied or not. Writing this way gives you the leftmost element satisfying the condition
[ 12:19 AM ] Siddharth Bhat : Can you please elaborate on what you mean why you say I treat it as if the condition is satisfied or not

[ 12:20 AM ] Shaanjeet : when you do this part ->
if (as[mid] >= val)


I like to treat it this way 

if (cond(mid))


where cond(i) returns true if the condition is satisfied or not. This type of binary search returns the leftmost element satisfying cond(i)
[ 12:22 AM ] Shaanjeet : condition in your case is as[mid] >= val
[ 12:22 AM ] Shaanjeet : it doesnt matter if you put it as > >=
[ 12:22 AM ] Siddharth Bhat : Right, thanks, that makes a lot of sense.
[ 12:22 AM ] Siddharth Bhat : And as for #1 and #3?
[ 12:23 AM ] Shaanjeet : it depends on implementation. I use my implementation because it always works [
12:23 AM ] Siddharth Bhat : Indeed.
[ 12:23 AM ] Shaanjeet :
i tried to eleminate the mid + 1 and mid -1 so that i dont have to think during a contest
[ 12:24 AM ] Shaanjeet : im not sure if it is good from theory point of view but in practise it always works for me
[ 12:24 AM ] Shaanjeet :
also i prefer to keep the bound always greater by 1, as right = right_range + 1.
[ 12:25 AM ] Shaanjeet : So that i can identify if there is no element that satisfy the condition in my range
[ 12:25 AM ] Shaanjeet : another good thing about my way is that at the end it gives you 2 values. right = leftmost element satisfying the condition and left = rightmost element not satisfying the condition
[ 12:26 AM ] Shaanjeet :
so i never have to change it for rightmost or leftmost
[ 12:26 AM ] Shaanjeet : it works almost as a template
[ 12:35 AM ] Siddharth Bhat : @Shaanjeet When you say

    right = leftmost element satisfying the condition, 
    left = rightmost element not satisfying the condition

Can you tell me what happens when my cond(mid)  = as[mid] <= 10, and I search on the array

as:  [1, 2, 10, 10, 10, 10, 10, 25, 35, 45
ix:   0  1  2   3   4   5   6   7   8    9
cond: T  T  T   T   T   T   T   F   F    F


using your implementation? Where will left and right be at the end?
Sorry, I found interpreting what you said hard.
[ 12:37 AM ] Shaanjeet : ok i shouldve mentioned that the condition should be of the form F F F F F F F T T T as i assume a increasing sequence
[ 12:37 AM ] Shaanjeet : which is easy to do by negating the cond(mid)
[ 12:38 AM ] Siddharth Bhat : Sure, let me take cond(mid) = mid >= 10?
[ 12:38 AM ] Siddharth Bhat :
as:  [1, 2, 10, 10, 10, 10, 10, 25, 35, 45
ix:   0  1  2   3   4   5   6   7   8    9
cond: F  F  T   T   T   T   T   T   T    T

[ 12:39 AM ] Siddharth Bhat : Now where will left and right be? Will it be left=2, right=3?
[ 12:39 AM ] Shaanjeet : no
[ 12:39 AM ] Shaanjeet : left = 1, right = 2
[ 12:40 AM ] Siddharth Bhat : argh yes :smile:
[ 12:41 AM ] Shaanjeet :

    int n;
    cin >> n;
    vector<int> arr(n);
    for (auto &it : arr) {
        cin >> it;
    }

    int left = -1, right = n;
    while (right - left > 1) {
        int mid = (left + right) / 2;
        if (arr[mid] >= 10) {
            right = mid;
        } else {
            left = mid;
        }
    }

    cout << left << " " << right << "\n";

[ 12:41 AM ] Shaanjeet : i used this to find it [
[ 12:42 AM ] Siddharth Bhat :
And if such an element does not exist, eg. something like:

as: [0, 0, 0, 0]
ix:  0  1  2  3
cond:F  F  F  F



I'll have right=4, left=3?
[ 12:42 AM ] Shaanjeet : yes
[ 12:43 AM ] Shaanjeet : and if it was T T T T then you would get left = -1
[ 12:43 AM ] Shaanjeet : and right = 0
[ 12:44 AM ] Siddharth Bhat : neat, thanks.
