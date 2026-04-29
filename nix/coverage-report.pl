#!/usr/bin/env perl
# Coverage threshold parser. Reads /tmp/cl-cc-coverage/cover-index.html,
# computes average expression and branch coverage across production rows, and
# fails if either falls below CL_CC_MIN_EXPR_COVERAGE / CL_CC_MIN_BRANCH_COVERAGE.
#
# The metric is intentionally production-only: tests, testing infrastructure, and
# package/export boilerplate are excluded. The defaults below are therefore a
# quality gate for executable production code, not a literal 100/100 line-count
# target across all source files.
use strict; use warnings;
my $path = q(/tmp/cl-cc-coverage/cover-index.html);
die "coverage index missing: $path\n" unless -f $path;
open my $fh, q(<), $path or die "open $path: $!\n";
local $/; my $html = <$fh>; close $fh;
die "coverage report is empty\n" if index($html, q(No code coverage data found)) >= 0;
my ($expr_sum, $expr_n, $branch_sum, $branch_n) = (0,0,0,0);
my $dir = q();
print "# Excluded test files from coverage average (production-only metric)\n";
while ($html =~ m{<tr class=.subheading.><td colspan=.7.>([^<]+)</td></tr>|<tr class=.(?:odd|even).><td class=.text-cell.><a [^>]+>([^<]+)</a></td><td>\s*([^<]+)</td><td>\s*([^<]+)</td><td>\s*([^<]+)</td><td>\s*([^<]+)</td><td>\s*([^<]+|-)</td><td>\s*([^<]+|-)</td>}g) {
  if (defined $1 && length $1) {
    $dir = $1;
    next;
  }
  my ($file, $expr_cov, $expr_total, $expr_pct, $branch_cov, $branch_total, $branch_pct) = ($2, $3, $4, $5, $6, $7, $8);
  next unless defined $file;
  my $full = $dir . $file;
  next if $full =~ m{(?:^|/)tests?/};
  next if $full =~ m{/packages/[^/]+/tests/};
  next if $full =~ m{/packages/testing/};
  next if $full =~ m{/(?:package\.lisp|exports-[^/]+\.lisp|facade-package-defpackage\.lisp|builtin-registry-data(?:-ext)?\.lisp|loop-data\.lisp|pipeline-(?:cps|data|repl-data)\.lisp|stdlib-source(?:-[^/]+)?\.lisp|prolog-data\.lisp|wasm-types\.lisp)$};
  if ($expr_pct ne q(-)) { $expr_sum += $expr_pct; $expr_n++; }
  if ($branch_pct ne q(-)) { $branch_sum += $branch_pct; $branch_n++; }
}
die "no production coverage rows parsed\n" unless $expr_n;
my $expr_avg = $expr_sum / $expr_n;
my $branch_avg = $branch_n ? ($branch_sum / $branch_n) : 0;
my $min_expr = $ENV{CL_CC_MIN_EXPR_COVERAGE} // 74;
my $min_branch = $ENV{CL_CC_MIN_BRANCH_COVERAGE} // 70;
printf "# Coverage averages: expr=%.2f%% branch=%.2f%%\n", $expr_avg, $branch_avg;
die sprintf("expression coverage %.2f%% is below threshold %.2f%%\n", $expr_avg, $min_expr)
  if $expr_avg < $min_expr;
die sprintf("branch coverage %.2f%% is below threshold %.2f%%\n", $branch_avg, $min_branch)
  if $branch_n && $branch_avg < $min_branch;
