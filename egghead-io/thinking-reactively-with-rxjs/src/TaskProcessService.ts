import { Observable, merge, Subject } from "rxjs";
import {
  distinctUntilChanged,
  filter,
  mapTo,
  pairwise,
  scan,
  shareReplay,
  startWith,
  switchMap,
  takeUntil,
} from "rxjs/operators";

const taskStarts = new Subject();
const taskCompletions = new Subject();
const showSpinner = new Subject();

export function newTaskStarted() {
  taskStarts.next();
}
export function existingTaskCompleted() {
  taskCompletions.next();
}

const loadUp = taskStarts.pipe(mapTo(1));
const loadDown = taskStarts.pipe(mapTo(-1));

const loadVariations = merge(loadUp, loadDown);
const currentLoadCount = loadVariations.pipe(
  startWith(0),
  scan((totalCurrentLoads, changeInLoads) => {
    const newLoadCount = totalCurrentLoads + changeInLoads;
    return newLoadCount < 0 ? 0 : newLoadCount;
  }),
  distinctUntilChanged(),
  shareReplay({ bufferSize: 1, refCount: true })
);

const shouldHideSpinner = currentLoadCount.pipe(filter((count) => count === 0));
const shouldShowSpinner = currentLoadCount.pipe(
  pairwise(),
  filter(([prevCount, currentCount]) => prevCount === 0 && currentCount === 1)
);

shouldShowSpinner
  .pipe(switchMap(() => showSpinner.pipe(takeUntil(shouldHideSpinner))))
  .subscribe();

export default {};
